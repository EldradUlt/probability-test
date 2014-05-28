{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}

module Test.ProbabilityCheck
       ( TestableDistribution
       , SampleableDistribution
       , testViaWilcoxMatchedPair
       , testApproximates
       , testSameConfidenceApproximates
       ) where

import Test.QuickCheck (Gen, generate, vectorOf, frequency)
import qualified Data.Vector.Unboxed as UV
import Statistics.Test.Types (TestType(..), TestResult(..))
import Statistics.Test.WilcoxonT (wilcoxonMatchedPairTest)
import Statistics.Test.MannWhitneyU (mannWhitneyUtest)
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty (TestTree)
import Numeric.SpecFunctions (choose)
import Data.Approximate (Approximate (..))
import Data.Ratio (numerator, denominator)

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
  return $ wilcoxonMatchedPairTest OneTailed p (UV.fromList $ map fst samples) (UV.fromList $ map snd samples)
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

-- A reasonable sample size to use for a desired Type I error rate,
-- Type II error rate, minimum meaningful difference, and the standard
-- deviation. For a OneTailed test between two normal distributions
-- where the test passes/fails based on whether the sample average is
-- greater than Za*stdDev/sqrt(sampleSize) where Za is the upper a
-- percentage point of the standard normal distribution.

minSampleSizeOneTailed :: Double -> Double -> Double -> Double -> Integer
minSampleSizeOneTailed alpha beta minDiff stdDev = ceiling $ ((upperPerOfNormDist alpha) - (inverseCumDist (1-beta)) / (minDiff/stdDev))^2

minSampleSizeTwoTailed :: Double -> Double -> Double -> Double -> Integer
minSampleSizeTwoTailed alpha = minSampleSizeOneTailed (alpha/2)

upperPerOfNormDist :: Double -> Double
upperPerOfNormDist alpha = undefined

-- This is called the Probit and can be numerically approximated.
inverseCumDist :: Double -> Double
inverseCumDist point = undefined


