{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, FlexibleContexts #-}

module Test.ProbabilityCheck
       ( TestableDistribution
       , SampleableDistribution
       , testNormDistSink
       , DistributionTestResult(..), DistributionTestValue(..)
       , testViaWilcoxMatchedPair
       , testApproximates
       , testSameConfidenceApproximates
       , minSampleSize
       , updateSSD, initSSD, StreamStdDev, conduitSSD
       ) where

import Test.QuickCheck (Gen, generate, vectorOf, frequency)
import qualified Data.Vector.Unboxed as UV
import Statistics.Test.Types (TestType(..), TestResult(..))
import Statistics.Test.WilcoxonT (wilcoxonMatchedPairSignificant) --wilcoxonMatchedPairTest)
import Statistics.Test.MannWhitneyU (mannWhitneyUtest)
import Statistics.Types (Sample)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty (TestTree)
import Numeric.SpecFunctions (choose)
import Data.Approximate (Approximate (..))
import Data.Ratio (numerator, denominator)
import qualified Data.Vector.Generic as G
import Data.Function (on)
import Statistics.Function (sortBy)
import Data.Ord (comparing)
import Numeric.Sum (kbn, sumVector)
import Data.Conduit (Sink, Conduit, await, yield)
import qualified Data.Conduit.List as CL
import Data.Number.Erf (invnormcdf, normcdf, Erf, InvErf)
import Control.Monad (void)

-- this class will have methods other than inspect however inspect
-- will always be a sufficient minimal instantiation.
class TestableDistribution dist where
  distMean :: dist -> Double
  distVariance :: dist -> Double

class SampleableDistribution s where
  sample :: s -> Gen Double

-- N >= ((z - inverse_Phi(1 - beta)) / (mu* / sigma))^2

--This is overly restirictive. Additionally it'll ungracefully throw
--an error if p is < 2^-1023. Also p should be restricted to
--0-1.
testViaWilcoxMatchedPair :: Double -> Gen (Double, Double) -> Gen (Maybe TestResult)
testViaWilcoxMatchedPair p genPair = do
  samples <- vectorOf sampleSize genPair
  return $ wilcoxonMatchedPairTest' OneTailed p (UV.fromList $ map fst samples) (UV.fromList $ map snd samples)
    where sampleSize = min 1023 ((ceiling $ logBase 2 (1/p)) * 10)

testApproximates ::(Ord a) => Double -> Gen (Approximate a, a) -> Gen (Maybe TestResult)
testApproximates p genApprox = 
  testViaWilcoxMatchedPair p $ genApprox >>= pairToGenDoubleP
    where pairToGenDoubleP (Approximate conf hi _ lo, actual) = return (fromRational $ toRational conf, if lo <= actual && actual <= hi then 1 else 0)

-- Obviously inefficient can be replaced.
smallestCentralBinomialCoefficientGreaterThan :: Double -> Int
smallestCentralBinomialCoefficientGreaterThan n = helper 1 n
  where helper a n = if ((2*a) `choose` a) > n then a else helper (a+1) n

-- Note this assumes that all the approximates have the same confidence.
testSameConfidenceApproximates :: (Ord a) => Double -> Gen ((Approximate a), a) -> Gen (Maybe TestResult)
testSameConfidenceApproximates p genApprox =
  do
    (conf, actuals) <- ((sequence $ take sampleSize $ repeat genApprox) >>= actualsToFirstConfAndActuals)
    expectedBools <- vectorOf sampleSize $
                     frequency [ (fromIntegral $ numerator conf, return True)
                               , (fromIntegral $ (denominator conf) - (numerator conf), return False)]
    return $ mannWhitneyUtest OneTailed p (boolsToSample expectedBools) (pairsToSample actuals)
      where sampleSize = (10 *) $ smallestCentralBinomialCoefficientGreaterThan (1/p)
            getConfidence ((Approximate c _ _ _):_) = toRational c
            boolsToSample = UV.fromList . (map (fromIntegral . fromEnum))
            pairToBool ((Approximate _ lo _ hi), a) = lo <= a && a <= hi
            pairsToSample = boolsToSample . map pairToBool
            actualsToFirstConfAndActuals lst@(((Approximate c _ _ _),_):_) = return (toRational c, lst)

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

data DistributionTestValue = TestSame
                           | TestSmaller
                           | TestGreater
                           | TestInsufficientSample
                           deriving (Show, Eq)

-- A reasonable sample size to use for a desired Type I error rate,
-- Type II error rate, minimum meaningful difference, and the standard
-- deviation. For a OneTailed test between two normal distributions
-- where the test passes/fails based on whether the sample average is
-- greater than Za*stdDev/sqrt(sampleSize) where Za is the upper a
-- percentage point of the standard normal distribution.

testNormDistSink :: (Erf a, InvErf a, RealFrac a, Ord a, Monad m) => a -> a -> a -> Sink a m (DistributionTestResult a)
testNormDistSink alpha beta minDiff = do
  mNext <- await
  case mNext of
    Nothing -> return $ DistributionTestResult TestInsufficientSample 0 0 0 0 0
    Just n -> testNormDistSink' alpha beta minDiff $ initSSD n

testNormDistSink' :: (Erf a, InvErf a, RealFrac a, Ord a, Monad m) =>
                     a -> a -> a -> StreamStdDev a -> Sink a m (DistributionTestResult a)
testNormDistSink' alpha beta minDiff ssd = do
  mNext <- await
  case mNext of
    Nothing -> return $ DistributionTestResult
               { dtrValue = TestInsufficientSample, dtrTestedMean = ssdMean ssd, dtrStdDev = ssdStdDev ssd
               , dtrSampleSize = ssdCount ssd, dtrUpperBound = 0, dtrLowerBound = 0}
    Just next -> if minSampleSize <= count
                 then return $ testNormalDistribution alpha stdDev count mean
                 else testNormDistSink' alpha beta minDiff newSSD
        where newSSD = updateSSD next ssd
              stdDev = ssdStdDev newSSD
              count = ssdCount newSSD
              mean = ssdMean newSSD
              minSampleSize = max (minSampleSizeTwoTailed alpha beta minDiff stdDev) 5

testNormalDistribution :: (Erf a, Ord a, Integral b) => a -> a -> b -> a -> DistributionTestResult a
testNormalDistribution alpha stdDev count actualDiff =
  if actualDiff > upperTest then res {dtrValue = TestGreater}
  else if actualDiff < lowerTest then res {dtrValue = TestSmaller}
       else res {dtrValue = TestSame}
    where upperTest = (upperPerOfNormDist alpha) * stdDev / (sqrt $ fromIntegral count)
          lowerTest = upperTest * (-1)
          res = DistributionTestResult { dtrValue = undefined, dtrTestedMean = actualDiff, dtrStdDev = stdDev
                                       , dtrSampleSize = fromIntegral count, dtrUpperBound = upperTest
                                       , dtrLowerBound = lowerTest}

minSampleSize :: (Erf a, InvErf a, RealFrac a, Integral b) => TestType -> a -> a -> a -> a -> b
minSampleSize testType = if testType == OneTailed then minSampleSizeOneTailed else minSampleSizeTwoTailed

minSampleSizeOneTailed :: (Erf a, InvErf a, RealFrac a, Integral b) => a -> a -> a -> a -> b
minSampleSizeOneTailed alpha beta minDiff stdDev = ceiling $ ((upperPerOfNormDist alpha) - (inverseCumDist (1-beta)) / (minDiff/stdDev))^2

minSampleSizeTwoTailed :: (Erf a, InvErf a, RealFrac a, Integral b) => a -> a -> a -> a -> b
minSampleSizeTwoTailed alpha = minSampleSizeOneTailed (alpha/2)

upperPerOfNormDist :: (Erf a) => a -> a
upperPerOfNormDist = normcdf

-- This is called the Probit and can be numerically approximated.
inverseCumDist :: (InvErf a) => a -> a
inverseCumDist = invnormcdf

data StreamStdDev a = StreamStdDev
    { ssdCount :: Integer
    , ssdMean :: a
    , ssdS :: a
    }
    deriving (Show)

ssdStdDev :: (Floating a) => StreamStdDev a -> a
ssdStdDev ssd = sqrt ((ssdS ssd) / ((fromIntegral $ ssdCount ssd) - 1))

initSSD :: (Num a) => a -> StreamStdDev a
initSSD x = StreamStdDev 1 x 0

updateSSD :: (Fractional a) => a -> StreamStdDev a -> StreamStdDev a
updateSSD x (StreamStdDev prevC prevM prevS) = StreamStdDev {ssdCount = newC, ssdMean = newM, ssdS = newS}
    where newC = prevC + 1
          newM = prevM + (x-prevM)/(fromIntegral newC)
          newS = prevS + (x-prevM)*(x-newM)

conduitSSD :: (Fractional a, Monad m) => Conduit a m (StreamStdDev a, a)
conduitSSD = do
  mFirst <- await
  case mFirst of
    Nothing -> return ()
    Just first -> do
      yield (initSSD first, first) 
      void $ CL.mapAccum updateSSDPair $ initSSD first
        where updateSSDPair a s = (updateSSD a s, (updateSSD a s, a))

{-
conduitSSD :: (Fractional a, Monad m) => Conduit a m (a, StreamStdDev a)
conduitSSD = do
  mNext <- await
  case mNext of
    Nothing -> return ()
    Just n -> do
      yield (n, initSSD n)
      conduitSSD' (initSSD n)

conduitSSD' :: (Fractional a, Monad m) => StreamStdDev a -> Conduit a m (a, StreamStdDev a)
conduitSSD' ssd = do
  mNext <- await
  case mNext of
    Nothing -> return ()
    Just n -> do
      yield (n, updateSSD n ssd)
      conduitSSD' $ updateSSD n ssd
-}

wilcoxonMatchedPairTest' :: TestType -> Double -> Sample -> Sample -> Maybe TestResult
wilcoxonMatchedPairTest' test p smp1 smp2 =
  wilcoxonMatchedPairSignificant test (min n1 n2) p
  $ wilcoxonMatchedPairSignedRank' smp1 smp2
    where
      n1 = UV.length smp1
      n2 = UV.length smp2

wilcoxonMatchedPairSignedRank' :: Sample -> Sample -> (Double, Double)
wilcoxonMatchedPairSignedRank' a b = (sum' ranks1, negate (sum' ranks2))
  where
    (ranks1, ranks2) = splitByTags
                       $ UV.zip tags (rank ((==) `on` abs) diffs)
    (tags,diffs) = UV.unzip
                   $ addTags                   -- Insert both positive and negative 0's and tag everything else positive or negative.
                   $ UV.span  (/= 0.0)         -- Divide out equal elements
                   $ sortBy (comparing abs)    -- Sort the differences by absolute difference
                   $ UV.zipWith (-) a b        -- Work out differences
    addTags (zeros,others) = (UV.++) (dupAndTag0s zeros) $ UV.map (\x -> (x>0 , x)) others
    dupAndTag0s zeros
      | UV.null zeros = UV.empty
      | otherwise = UV.cons (True, 0) $ UV.cons (False, 0) $ dupAndTag0s $ UV.tail zeros
  
-- Private data type for unfolding
data Rank v a = Rank {
        rankCnt :: {-# UNPACK #-} !Int        -- Number of ranks to return
    , rankVal :: {-# UNPACK #-} !Double     -- Rank to return
    , rankNum :: {-# UNPACK #-} !Double     -- Current rank
    , rankVec :: v a                        -- Remaining vector
    }
                
                -- | Calculate rank of sample. Sample should be already sorted.
rank :: (G.Vector v a, G.Vector v Double)
        => (a -> a -> Bool)        -- ^ Equivalence relation
        -> v a                     -- ^ Vector to rank
        -> v Double
rank eq vec = G.unfoldr go (Rank 0 (-1) 1 vec)
  where
    go (Rank 0 _ r v)
      | G.null v  = Nothing
      | otherwise =
          case G.length h of
            1 -> Just (r, Rank 0 0 (r+1) rest)
            n -> go Rank { rankCnt = n
                         , rankVal = 0.5 * (r*2 + fromIntegral (n-1))
                         , rankNum = r + fromIntegral n
                         , rankVec = rest
                         }
          where
            (h,rest) = G.span (eq $ G.head v) v
    go (Rank n val r v) = Just (val, Rank (n-1) val r v)

splitByTags :: (G.Vector v a, G.Vector v (Bool,a)) => v (Bool,a) -> (v a, v a)
splitByTags vs = (G.map snd a, G.map snd b)
  where
    (a,b) = G.unstablePartition fst vs

sum' :: (G.Vector v Double) => v Double -> Double
sum' = sumVector kbn


