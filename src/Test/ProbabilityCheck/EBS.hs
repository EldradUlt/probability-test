{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Test.ProbabilityCheck.EBS
       ( DistributionTestResult(..), DistributionTestValue(..)
       , conduitPrint
       , empiricalBernstienStopping
       ) where

import Data.Conduit (Sink, Conduit, await, yield, (=$), awaitForever, addCleanup)
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class (MonadIO(..))
import Data.Ratio ((%))
import System.IO ( hSetBuffering, BufferMode(NoBuffering), stdout)
import System.ProgressBar (progressBar, msg, exact)
import Control.Concurrent (threadDelay)

import Test.ProbabilityCheck.Types

-- | Debugging helper function which is currently not used and should
-- probably be removed.
conduitPrint :: (Show a, MonadIO m) => Conduit a m a
conduitPrint = CL.mapM (\x -> do
                           liftIO (print x) 
                           return x)

-- http://machinelearning.org/archive/icml2008/papers/523.pdf
--
-- Assertions I believe can be made, if range is the magnitude of
-- range of possible values in the population.
--
-- 1) This will stop in finite time.
--
-- 2) If the actual mean of the population is 0 then the test will
-- return TestZero at least (1-delta) of the time.
--
-- 3) If the absolute value of the actual mean of the population is
-- greater than eps the test will return the appropriate TestPositive
-- or TestNegative at least (1-delta) of the time.
empiricalBernstienStopping :: (Show a, RealFrac a, Floating a, Ord a, MonadIO m) => a -> a -> a -> Sink a m (DistributionTestResult a)
empiricalBernstienStopping range delta eps = do
  result <- ssdConduit
            =$ (CL.drop 1 >> awaitForever yield)
            =$ empiricalBernstienStoppingConduit 2 1 range delta
            =$ printEBSConduit range delta eps
            =$ empiricalBernstienStoppingSink eps
  return result

empiricalBernstienStoppingConduit :: forall a m. (RealFrac a, Floating a, Ord a, Monad m)
                                     => Integer -> Integer -> a -> a -> Conduit (StreamStdDev a) m (EBSState a)
empiricalBernstienStoppingConduit t k range delta = do
  mNext <- await
  case mNext of
    Nothing -> return ()
    Just ssd -> do
      yield $ EBSState {ebsSSD = ssd, ebsCt = ct, ebsT = newT, ebsK = newK, ebsX = x, ebsDk = dk, ebsAlpha = alpha}
      empiricalBernstienStoppingConduit newT newK range delta
      where  ct :: a
             ct = (ssdStdDev ssd)*sqrt(2*x/(fromInteger t)) + 3*range*x/(fromInteger t)
             beta :: a
             beta = 1.1 -- I'm not sure what this value wants to
                        -- be. Any value greater than 1 should be
                        -- valid. But I don't know what values are
                        -- optimal.
             alpha :: Rational
             alpha = floor(beta^k) % floor(beta^(k-1))
             x :: a
             x = (-1) * (fromRational alpha) * log (dk / 3)
             dk :: a
             -- This actually converges to exactly delta instead of
             -- slightly less than delta (~.96*delta) for the
             -- commented out values.
             dk = delta / (fromIntegral $ k*(k+1))
             {-
             dk = c / (fromIntegral k) ** p
             p :: a
             p = 1.1
             c :: a
             c = delta * (p-1) / p
             -}
             newT = t+1
             newK = if newT > floor(beta^k) then k+1 else k

empiricalBernstienStoppingSink :: (RealFrac a, Floating a, Ord a, Monad m) => a -> Sink (EBSState a) m (DistributionTestResult a)
empiricalBernstienStoppingSink eps = do
  mNext <- await
  case mNext of
    Nothing -> return $ DistributionTestResult
               -- These SSD values are a lie but I don't want to keep
               -- the previous ssd around just for that. Probably will
               -- eventually or will peek at the next.
               { dtrValue = TestInsufficientSample, dtrTestedMean = 0, dtrStdDev = 0
               , dtrSampleSize = 0, dtrUpperBound = 0, dtrLowerBound = 0}
    Just ebs ->
      case (abs mean, ct) of
        (absMean, bound) | (absMean + bound < eps) ->
          return $ dtr TestZero
        (absMean, bound) | (absMean - bound) > 0 ->
          return $ dtr $ if mean > 0 then TestPositive else TestNegative
        _ -> empiricalBernstienStoppingSink eps
      where ssd = ebsSSD ebs
            ct = ebsCt ebs
            dtr a = DistributionTestResult { dtrValue = a
                                           , dtrTestedMean = mean
                                           , dtrStdDev = stdDev
                                           , dtrSampleSize = count
                                           , dtrUpperBound = mean + ct
                                           , dtrLowerBound = mean - ct }
            mean = ssdMean ssd
            stdDev = ssdStdDev ssd
            count = ssdCount ssd

printEBSConduit :: (Show a, RealFrac a, Floating a, MonadIO m) => a -> a -> a -> Conduit (EBSState a) m (EBSState a)
printEBSConduit range delta eps = do
  liftIO $ hSetBuffering stdout NoBuffering
  liftIO $ threadDelay 10000 -- This is a hack to prevent a collision
                             -- of printing with hunit. Should be
                             -- fixed.
  liftIO $ putStrLn ""
  addCleanup (\_-> liftIO$ putStr ": ") $ awaitForever go
    where go ebs@(EBSState ssd _ t k _ _ _) = do
            liftIO $ progressBar (msg "Working") exact 40 t estimatedEnd
            yield ebs
            where estimatedEnd = ceiling $ (b + sqrt(b^(2::Integer)+4*a*c))^(2::Integer)/(4*a^(2::Integer))
                  a = max (abs $ ssdMean ssd) (abs $ (abs $ ssdMean ssd) - eps)
                  b = (ssdStdDev ssd) * sqrt(2*1.1*log((fromInteger $ 3*k*(k+1))/delta))
                  c = 3*1.1*range*log((fromInteger $ 3*k*(k+1))/delta)


