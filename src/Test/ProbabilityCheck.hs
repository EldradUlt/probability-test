{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, FlexibleContexts #-}

module Test.ProbabilityCheck
       ( testNormDistSink
       , wilcoxonSink, wilcoxonRankedPairsConduit
       , DistributionTestResult(..), DistributionTestValue(..), DistributionTestSummary(..), initDTS
       , updateSSD, ssdConduit
       ) where

import Statistics.Test.Types (TestType(..))
import Data.Conduit (Sink, Conduit, await, yield, (=$=), (=$))
import qualified Data.Conduit.List as CL
import Data.Number.Erf (invnormcdf, InvErf)
import Control.Monad (void)
import Data.Map.Strict (Map, singleton)
import Data.List (sort, groupBy)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Time (getCurrentTime, UTCTime, diffUTCTime)

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

initDTS :: (Num a) => DistributionTestResult a -> DistributionTestSummary a
initDTS (DistributionTestResult val mean stddev size upper lower) = 
  DistributionTestSummary (singleton val 1) (initSSD mean) (initSSD stddev) (initSSD $ fromIntegral size) (initSSD upper) (initSSD lower)

printTestInfo :: (Show a, InvErf a, RealFrac a, MonadIO m) => a -> a -> Conduit (StreamStdDev a) m (StreamStdDev a)
printTestInfo alpha minDiff = do
  startTime <- liftIO getCurrentTime
  void $ CL.mapAccumM foobar (0, startTime)
    where foobar ssd (prevC, prevTS) = do
            now <- liftIO getCurrentTime
            if reportNeeded
              then do
              liftIO $ print report
              return ((ssdCount ssd, now), ssd)
              else return ((prevC, prevTS), ssd)
          reportNeeded = undefined
          report = "undefined"

testNormDistSink :: (InvErf a, RealFrac a, Ord a, Monad m) => a -> a -> Sink a m (DistributionTestResult a)
testNormDistSink alpha minDiff = ssdConduit =$ (testNormDistSink' alpha minDiff)

testNormDistSink' :: (InvErf a, RealFrac a, Ord a, Monad m) => a -> a -> Sink (StreamStdDev a) m (DistributionTestResult a)
testNormDistSink' alpha minDiff = do
  mNext <- await
  case mNext of
    Nothing -> return $ DistributionTestResult
               -- These SSD values are a lie but I don't want to keep
               -- the previous ssd around just for that. Probably will
               -- eventually or will peek at the next.
               { dtrValue = TestInsufficientSample, dtrTestedMean = 0, dtrStdDev = 0
               , dtrSampleSize = 0, dtrUpperBound = 0, dtrLowerBound = 0}
    Just ssd -> if minSampleSize TwoTailed alpha minDiff stdDev <= count
                then return $ testNormalDistribution alpha stdDev count $ ssdMean ssd
                else testNormDistSink' alpha minDiff
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

minSampleSize :: (InvErf a, RealFrac a, Integral b) => TestType -> a -> a -> a -> b
minSampleSize testType = if testType == OneTailed then minSampleSizeOneTailed else minSampleSizeTwoTailed

-- Putting a hard limit of 20 here is just temporary to work around inaccuracy of estimating the 
minSampleSizeOneTailed :: (InvErf a, RealFrac a, Integral b) => a -> a -> a -> b
minSampleSizeOneTailed alpha minDiff stdDev = max 20 $ ceiling $ ((upperPerOfNormDist alpha) / (minDiff/stdDev))^(2::Integer)

minSampleSizeTwoTailed :: (InvErf a, RealFrac a, Integral b) => a -> a -> a -> b
minSampleSizeTwoTailed alpha = minSampleSizeOneTailed (alpha/2)

upperPerOfNormDist :: (InvErf a) => a -> a
upperPerOfNormDist = invnormcdf . (1 - )

-- This probably all wants to be moved to another file.
data StreamStdDev a = StreamStdDev
    { ssdCount :: Integer
    , ssdMean :: a
    , ssdS :: a
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
        where updateSSDPair a s = (updateSSD a s, (updateSSD a s, a))

wilcoxonSink :: (InvErf a, RealFrac a, Ord a, Monad m) => a -> a -> Sink (a,a) m (DistributionTestResult a)
wilcoxonSink alpha minDiff = wilcoxonRankedPairsConduit =$ testNormDistSink alpha minDiff

wilcoxonRankedPairsConduit :: (InvErf a, RealFrac a, Ord a, Monad m) => Conduit (a,a) m a
wilcoxonRankedPairsConduit = (CL.map $ uncurry (-)) =$= wilcoxonRankedConduit' 40

wilcoxonRankedConduit' :: (InvErf a, RealFrac a, Ord a, Monad m) => Int -> Conduit a m a
wilcoxonRankedConduit' size = do
  lst <- CL.take size
  if length lst == size then 
    let pLst = sort $  map (\a -> (abs a, signum a)) lst
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


