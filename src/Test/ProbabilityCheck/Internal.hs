{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

-- | This contains the algorithm used internally by ProbabilityCheck
-- and TestApproximate.
module Test.ProbabilityCheck.Internal
       ( DistributionTestResult(..)
       , DistributionTestValue(..)
       , empiricalBernstienStopping
       , monadicToSource
       ) where

import Data.Conduit (Sink, Conduit, Source, await, yield, (=$), (=$=), awaitForever, addCleanup)
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class (MonadIO(..))
import Data.Ratio ((%))
import System.IO ( hSetBuffering, BufferMode(NoBuffering), stdout)
import System.ProgressBar (progressBar, msg, exact)
import Control.Concurrent (threadDelay)
import Control.Monad (void)

-- | Algorithm used internally to determine how many samples are
-- needed and whether the tested mean is equal to zero or has an
-- absolute value greater than epsilon.
--
-- This is a slight modification of the algorithm described in the
-- following paper:
--
-- http://machinelearning.org/archive/icml2008/papers/523.pdf
--
-- The following assertions can be made.
--
-- 1) This will stop in finite time.
--
-- 2) If the actual mean of the population is 0 then the test will
-- return TestZero at least (1-delta) of the time.
--
-- 3) If the absolute value of the actual mean of the population is
-- greater than eps the test will return the appropriate TestPositive
-- or TestNegative at least (1-delta) of the time.
empiricalBernstienStopping :: (Show a, RealFrac a, Floating a, Ord a, MonadIO m)
                              => a -- ^ The size of the range that all
                                   -- possible input values will fall
                                   -- within.
                              -> a -- ^ 'Delta' the allowable error rate for the test.
                              -> a -- ^ 'Epsilon' the minimal
                                   -- difference between the tested
                                   -- mean and zero to be considered
                                   -- not equal to zero.
                              -> Sink a m (DistributionTestResult a)
                              -- ^ A sink which when attached to a
                              -- source of values will test whether
                              -- they have a mean equal to, greater
                              -- than, or less than zero.
empiricalBernstienStopping range delta eps = do
  result <- ssdConduit
            =$ (CL.drop 1 >> awaitForever yield)
            =$ empiricalBernstienStoppingConduit 2 1 range delta
            =$ printEBSConduit range delta eps
            =$ empiricalBernstienStoppingSink eps
  return result

-- Conduit which handles the calculations between each step.
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

-- Sink which checks if the stopping condition has been met and if so
-- returns the results.
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

-- Printing function which displays progress and estimated
-- termination.
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

-- | The result of a test.
data DistributionTestResult a =
  DistributionTestResult
  { dtrValue :: DistributionTestValue -- ^ The assertion that the mean
                                      -- is greater than, less than,
                                      -- or equal to zero.
  , dtrTestedMean :: a -- ^ The mean of the observed samples.
  , dtrStdDev :: a     -- ^ The standard devitation of the observed
                       -- samples.
  , dtrSampleSize :: Integer -- ^ Number of samples taken.
  , dtrUpperBound :: a -- ^ The upper bound which can be asserted with
                       -- the requested confidence for the
                       -- population's mean.
  , dtrLowerBound :: a -- ^ The lower bound which can be asserted with
                       -- the requested confidence for the
                       -- population's mean.
  }
  deriving (Show, Eq) 

-- | Possible values for a test result.
data DistributionTestValue =
    TestZero     -- ^ Assert that the mean is equal to zero.
  | TestNegative -- ^ Assert that the mean is less than zero.
  | TestPositive -- ^ Assert that the mean is greater than zero.
  | TestInsufficientSample -- ^ Not enough samples available.
  deriving (Show, Eq, Ord)

-- Helper data type for keeping a running mean and standard deviation
-- (Knuth 1998, The Art of Computer Programming).
data StreamStdDev a = StreamStdDev
    { ssdCount :: !Integer -- ^ Total elements seen.
    , ssdMean :: !a -- ^ Mean.
    , ssdS :: !a -- ^ Internal value from which the stddev is calculated.
    }
    deriving (Eq)

instance (Show a, Floating a) => Show (StreamStdDev a) where
  show ssd@(StreamStdDev count mean s) =
    "StreamStdDev {ssdCount = " ++ (show count)
    ++ ", ssdMean = " ++ (show mean)
    ++ ", ssdStdDev = " ++ (show $ ssdStdDev ssd)
    ++ ", ssdS = " ++ (show s)
    ++ "}"

-- Accessor for virtual stddev value in the StreamStdDev type.
ssdStdDev :: (Floating a) => StreamStdDev a -> a
ssdStdDev ssd = sqrt ((ssdS ssd) / ((fromIntegral $ ssdCount ssd) - 1))

-- Create a StreamStdDev from the first value.
initSSD :: (Num a) => a -> StreamStdDev a
initSSD x = StreamStdDev 1 x 0

-- Insert a single value into a StreamStdDev
updateSSD :: (Fractional a) => a -> StreamStdDev a -> StreamStdDev a
updateSSD x (StreamStdDev prevC prevM prevS) = StreamStdDev {ssdCount = newC, ssdMean = newM, ssdS = newS}
    where newC = prevC + 1
          newM = prevM + (x-prevM)/(fromIntegral newC)
          newS = prevS + (x-prevM)*(x-newM)

-- Takes upstream numerical values and passes the count, mean, and
-- stddev of all values seen thus far down stream.
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

-- State used internally by empiricalBernstienStoppingConduit between
-- each step.
data EBSState a = EBSState
    { ebsSSD :: StreamStdDev a
    , ebsCt :: a
    , ebsT :: Integer
    , ebsK :: Integer
    , ebsX :: a
    , ebsDk :: a
    , ebsAlpha :: Rational
    } deriving (Show)

-- | A helper function which turns a monadic value into a
-- Data.Conduit.Source of those values.
monadicToSource :: (Monad m) => m a -> Source m a
monadicToSource ma = CL.unfoldM (\_ -> ma >>= (\a -> return $ Just (a,()))) ()


