{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Test.ProbabilityCheck
       ( DistributionTestResult(..), DistributionTestValue(..)
       , updateSSD, ssdConduit
       , conduitPrint
       , empiricalBernstienStopping
       ) where

import Data.Conduit (Sink, Conduit, await, yield, (=$=), (=$), awaitForever)
import qualified Data.Conduit.List as CL
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Ratio ((%))

-- This probably wants better naming at some point.
data DistributionTestResult a = DistributionTestResult
                                { dtrValue :: DistributionTestValue
                                , dtrTestedMean :: a
                                , dtrStdDev :: a
                                , dtrSampleSize :: Integer
                                , dtrUpperBound :: a
                                , dtrLowerBound :: a
                                }
                              deriving (Show, Eq) 

data DistributionTestValue = TestZero
                           | TestNegative
                           | TestPositive
                           | TestInsufficientSample
                           deriving (Show, Eq, Ord)

-- StreamStdDev and its basic functions probably want to be moved to
-- another file.
data StreamStdDev a = StreamStdDev
    { ssdCount :: !Integer
    , ssdMean :: !a
    , ssdS :: !a
    }
    deriving (Eq)

instance (Show a, Floating a) => Show (StreamStdDev a) where
  show ssd@(StreamStdDev count mean s) =
    "StreamStdDev {ssdCount = " ++ (show count)
    ++ ", ssdMean = " ++ (show mean)
    ++ ", ssdStdDev = " ++ (show $ ssdStdDev ssd)
    ++ ", ssdS = " ++ (show s)
    ++ "}"

ssdStdDev :: (Floating a) => StreamStdDev a -> a
ssdStdDev ssd = sqrt ((ssdS ssd) / ((fromIntegral $ ssdCount ssd) - 1))

initSSD :: (Num a) => a -> StreamStdDev a
initSSD x = StreamStdDev 1 x 0

updateSSD :: (Fractional a) => a -> StreamStdDev a -> StreamStdDev a
updateSSD x (StreamStdDev prevC prevM prevS) = StreamStdDev {ssdCount = newC, ssdMean = newM, ssdS = newS}
    where newC = prevC + 1
          newM = prevM + (x-prevM)/(fromIntegral newC)
          newS = prevS + (x-prevM)*(x-newM)

ssdConduit :: (Fractional a, Monad m) => Conduit a m (StreamStdDev a)
ssdConduit = do
  mFirst <- await
  case mFirst of
    Nothing -> return ()
    Just first -> do
      yield (initSSD first)
      void (CL.mapAccum updateSSDPair $ initSSD first) =$= CL.map fst
        where updateSSDPair a s = (newSSD, (newSSD, a))
                where newSSD = updateSSD a s

-- | Debugging helper function which is currently not used and should
-- probably be removed.
conduitPrint :: (Show a, MonadIO m) => Conduit a m a
conduitPrint = CL.mapM (\x -> do
                           liftIO (print x) 
                           return x)

-- http://machinelearning.org/archive/icml2008/papers/523.pdf
empiricalBernstienStopping :: (Show a, RealFrac a, Floating a, Ord a, MonadIO m) => a -> a -> a -> Sink a m (DistributionTestResult a)
empiricalBernstienStopping range delta eps =
     ssdConduit
  =$ do {CL.drop 1; awaitForever yield}
  =$ empiricalBernstienStoppingConduit 2 1 range delta
--  =$ conduitPrint
  =$ empiricalBernstienStoppingSink eps

data EBSState a = EBSState
    { ebsSSD :: StreamStdDev a
    , ebsCt :: a
    , ebsT :: Integer
    , ebsK :: Integer
    , ebsX :: a
    , ebsDk :: a
    , ebsAlpha :: Rational
    } deriving (Show)

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
             beta = 1.1 -- This probably wants to be proportional to
                    -- delta or eps. It is accurate but non-optimal as
                    -- is.
             alpha :: Rational -- This only needs to be recalculated
                      -- when k changes.
             alpha = floor(beta^k) % floor(beta^(k-1))
             x :: a -- This only needs to be recaclulated when k
                  -- changes.
             x = (-1) * (fromRational alpha) * log (dk / 3)
             dk :: a
             -- dk = c / (fromIntegral k) ** p
             
             -- This actually converges to exactly delta instead of
             -- slightly less than delta (~.96*delta) for the
             -- commented out values.
             dk = delta / (fromIntegral $ k*(k+1))
             {-
             p :: a
             p = 1.1
             c :: a
             c = delta * (p-1) / p
             -}
             newT = t+1
             newK = if t+1 > floor(beta^k) then k+1 else k

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
          return $ dtr { dtrValue = TestZero }
        (absMean, bound) | (absMean - bound) > 0 ->
          return $ dtr { dtrValue = if mean > 0 then TestPositive else TestNegative }
        _ -> empiricalBernstienStoppingSink eps
      where ssd = ebsSSD ebs
            ct = ebsCt ebs
            dtr = DistributionTestResult { dtrValue = error "Used DistributionTestResult without setting value."
                                         , dtrTestedMean = mean
                                         , dtrStdDev = stdDev
                                         , dtrSampleSize = count
                                         , dtrUpperBound = mean + ct
                                         , dtrLowerBound = mean - ct }
            mean = ssdMean ssd
            stdDev = ssdStdDev ssd
            count = ssdCount ssd


