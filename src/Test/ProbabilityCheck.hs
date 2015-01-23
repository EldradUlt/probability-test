{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Test.ProbabilityCheck
       ( testNormDistSink
       , wilcoxonSink, wilcoxonRankedPairsConduit
       , DistributionTestResult(..), DistributionTestValue(..), DistributionTestSummary(..), initDTS
       , updateSSD, ssdConduit
       , MinDiff(..)
       , conduitPrint
       ) where

import Statistics.Test.Types (TestType(..))
import Data.Conduit (Sink, Conduit, await, yield, (=$=), (=$), awaitForever, addCleanup)
import qualified Data.Conduit.List as CL
import Data.Number.Erf (invnormcdf, InvErf)
import Control.Monad (void)
import Data.Map.Strict (Map, singleton)
import Data.List (sort, groupBy)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Time (getCurrentTime, diffUTCTime, addUTCTime, NominalDiffTime, utcToZonedTime, getTimeZone, getTimeZone)
import Control.Concurrent (threadDelay)

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

data DistributionTestSummary a = DistributionTestSummary
                                 { dtsValues :: Map DistributionTestValue Integer
                                 , dtsMeans :: StreamStdDev a
                                 , dtsStdDevs :: StreamStdDev a
                                 , dtsSampleSizes :: StreamStdDev a
                                 , dtsUpperBounds :: StreamStdDev a
                                 , dtsLowerBounds :: StreamStdDev a
                                 } deriving (Show, Eq)

data MinDiff a = MDAbsolute a
               | MDRelative a

initDTS :: (Num a) => DistributionTestResult a -> DistributionTestSummary a
initDTS (DistributionTestResult val mean stddev size upper lower) = 
  DistributionTestSummary (singleton val 1) (initSSD mean) (initSSD stddev) (initSSD $ fromIntegral size) (initSSD upper) (initSSD lower)

printTestInfo :: (Show a, InvErf a, RealFrac a, MonadIO m)
                 => Conduit (StreamStdDev a, Integer) m (StreamStdDev a, Integer)
printTestInfo = do
  startTime <- liftIO getCurrentTime
  liftIO $ threadDelay 10000 -- This prevents some printing collision issues but could slow things down if called a lot for some reason.
  liftIO $ putChar '\n'
  addCleanup (\_ -> liftIO $ putChar '\n') $ void $ CL.mapAccumM (go startTime) (0, startTime, 0)
    where go _ i@(ssd, stopC) (prevC, prevTS, prevRL) =
            do now <- liftIO getCurrentTime
               tz <- liftIO $ getTimeZone now
               let reportNeeded = lastReportLongAgo || hitMilestone
                   lastReportLongAgo = diffUTCTime now prevTS > 1 -- It has been more than 1 second since the last report.
                   hitMilestone = curC >= stopC
                   report' = "Completed " ++ (show curC)
                             ++ "/" ++ (show $ stopC)
                             ++ " iter (" ++ (show $ div (100 * curC) stopC)
                             ++ "%), at " ++ (show (round speed :: Integer)) -- This probably wants diff display.
                             ++ " iter/s, finish in " ++ (show (round estimatedTimeToFinish :: Integer)) -- Same
                             ++ " s (" ++ (show estimatedDateOfFinish) ++ ")."
                   report = if length report' < prevRL then report' ++ (replicate (prevRL - (length report')) ' ') else report'
                   curC = ssdCount ssd
                   speed = (fromIntegral $ curC - prevC) / (realToFrac $ diffUTCTime now prevTS) :: Double
                   estimatedTimeToFinish = realToFrac $ (fromIntegral $ stopC - curC) / speed :: NominalDiffTime
                   estimatedDateOfFinish = utcToZonedTime tz $ addUTCTime estimatedTimeToFinish now
                   runReport = do
                     liftIO $ putChar '\r' >> putStr report
                     return ((ssdCount ssd, now, length report), i)
                 in if reportNeeded then runReport else return ((prevC, prevTS, prevRL), i)

-- | Creates a Sink which when fused with a Source with the given
-- properties will return a DistributionTestResult whose dtsValue is
-- a particular value the given percentage of the time:
-- 
-- If the Source closes before enough samples have been taken the
-- resulting value will be TestInsufficientSample.
--
-- If the Source is a Normal Distribution with a mean of zero then the
-- resulting value will be TestZero the desired confidence
-- percentage of the time.
--
-- If the Source is a Normal Distribution with a mean of +/- the
-- specified minimal difference then the resulting value will be
-- TestPositive/TestNegative the desired confidence percentage of the
-- time. If absolute value of the Source's mean is greater than the
-- specified minimal difference then the applicable
-- TestPositive/TestNegative result will occur more often than the
-- desired confidence.
--
-- If the Source is a Normal Distribution with a mean strictly between
-- +/- the specified minimal difference, but not zero then there are
-- no guarantees of the results.
--
-- If the Source is not a Normal Distribution there are no guarantees
-- of the results.
--
-- /Bug/: #1 Test result actually has a lower confidence and a larger
-- minimal difference than the values given.
--
-- /Bug/: #5 There's a mild performance and reporting bug in the
-- printing which may cause race conditions where printing of the
-- test's name is interwoven with updates of the running
-- test. Additionally the fix in place to make that unlikely will
-- cause a performance hit when large numbers of small tests are run.
testNormDistSink ::
  (InvErf a, RealFrac a, Ord a, Show a, MonadIO m)
  => Bool -- ^ Whether to print updates as test is running. (Bug #5)
  -> a -- ^ This is 1 minus the desired confidence, which is to say the allowable inaccuracy rate. E.g. 0.05. (Bug #1)
  -> MinDiff a -- ^ Minimal difference to be considered different. (Bug #1)
  -> Sink a m (DistributionTestResult a)
testNormDistSink prnt alpha minDiff =    ssdConduit
                                      =$ ssdToSSDandEnd alpha minDiff
                                      =$ (if prnt then printTestInfo else CL.map id)
                                      =$ testNormDistSink' alpha

ssdToSSDandEnd :: forall a b m. (InvErf a, RealFrac a, Ord a, Integral b, Monad m) =>
                  a -> MinDiff a -> Conduit (StreamStdDev a) m (StreamStdDev a, b)
ssdToSSDandEnd alpha minDiff = awaitForever givePair
  where givePair :: StreamStdDev a -> Conduit (StreamStdDev a) m (StreamStdDev a, b)
        givePair ssd = yield (ssd, minSampleSize TwoTailed alpha minDiff $ (ssdStdDev ssd :: a))

testNormDistSink' :: (InvErf a, RealFrac a, Ord a, Monad m) => a -> Sink (StreamStdDev a, Integer) m (DistributionTestResult a)
testNormDistSink' alpha = do
  mNext <- await
  case mNext of
    Nothing -> return $ DistributionTestResult
               -- These SSD values are a lie but I don't want to keep
               -- the previous ssd around just for that. Probably will
               -- eventually or will peek at the next.
               { dtrValue = TestInsufficientSample, dtrTestedMean = 0, dtrStdDev = 0
               , dtrSampleSize = 0, dtrUpperBound = 0, dtrLowerBound = 0}
    Just (ssd, endC) -> if endC <= count
                        then return $ testNormalDistribution alpha stdDev count $ ssdMean ssd
                        else testNormDistSink' alpha
                          where stdDev = ssdStdDev ssd
                                count = ssdCount ssd

testNormalDistribution :: (InvErf a, Ord a, Integral b) => a -> a -> b -> a -> DistributionTestResult a
testNormalDistribution alpha stdDev count actualDiff =
  if actualDiff > upperTest then res {dtrValue = TestPositive}
  else if actualDiff < lowerTest then res {dtrValue = TestNegative}
       else res {dtrValue = TestZero}
    where upperTest = (upperPerOfNormDist alpha) * stdDev / (sqrt $ fromIntegral count)
          lowerTest = upperTest * (-1)
          res = DistributionTestResult { dtrValue = error "DistributionTestResult's value was not written in."
                                       , dtrTestedMean = actualDiff, dtrStdDev = stdDev
                                       , dtrSampleSize = fromIntegral count, dtrUpperBound = upperTest
                                       , dtrLowerBound = lowerTest}

minSampleSize :: (InvErf a, RealFrac a, Integral b) => TestType -> a -> MinDiff a -> a -> b
minSampleSize testType = if testType == OneTailed then minSampleSizeOneTailed else minSampleSizeTwoTailed

-- The hard coded minimum of 20 smaples is arbitrary. Should be thought about and justified.
minSampleSizeOneTailed :: (InvErf a, RealFrac a, Integral b) => a -> MinDiff a -> a -> b
minSampleSizeOneTailed alpha minDiff stdDev = max 20 $ ceiling $ ((upperPerOfNormDist alpha) / mdOverSD)^(2::Integer)
  where mdOverSD = case minDiff of
          MDAbsolute md -> md / stdDev
          MDRelative md -> md

minSampleSizeTwoTailed :: (InvErf a, RealFrac a, Integral b) => a -> MinDiff a -> a -> b
minSampleSizeTwoTailed alpha = minSampleSizeOneTailed (alpha/2)

upperPerOfNormDist :: (InvErf a) => a -> a
upperPerOfNormDist = invnormcdf . (1 - )

-- This probably all wants to be moved to another file.
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

wilcoxonSink :: (InvErf a, RealFrac a, Ord a, Show a, MonadIO m) => Int -> a -> a -> Sink (a,a) m (DistributionTestResult a)
wilcoxonSink size alpha minDiff = wilcoxonRankedPairsConduit size =$ testNormDistSink True alpha (MDRelative minDiff)

wilcoxonRankedPairsConduit :: (InvErf a, RealFrac a, Ord a, Show a, Monad m) => Int -> Conduit (a,a) m a
wilcoxonRankedPairsConduit size = (CL.map $ uncurry (-))
                                  =$= wilcoxonRankedConduit' size

-- | Debugging helper function which is currently not used and should
-- probably be removed.
conduitPrint :: (Show a, MonadIO m) => Conduit a m a
conduitPrint = CL.mapM (\x -> do
                           liftIO (print x) 
                           return x)

wilcoxonRankedConduit' :: (InvErf a, RealFrac a, Ord a, Show a, Monad m) => Int -> Conduit a m a
wilcoxonRankedConduit' size = do
  lst <- CL.take size
  if length lst == size then 
    let pLst = sort $ map (\a -> (abs a, signum a)) lst
        n = fromIntegral $ length pLst
        rankedLst = assignRanks 0 $ groupBy (\p1 p2 -> (fst p1) == (fst p2)) pLst
        assignRanks _ [] = []
        assignRanks cnt (r:rest) = (map (\(_, zi) -> zi * (2*cnt+1+(fromIntegral $ length r)) / 2) r) ++ (assignRanks (cnt+1) rest)
        pPos = (fromIntegral (length $ filter ((1==) . snd) pLst)) / n
        pNeg = (fromIntegral (length $ filter (((-1)==) . snd) pLst)) / n
        t = sum rankedLst
        testValue = t / (sqrt $ ((2*n*(n+1)*(2*n+1)) / 3) * (pPos + pNeg - ((pPos - pNeg)^(2::Integer))))
    in do
      yield testValue
      wilcoxonRankedConduit' size
    else return ()


