{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables #-}

module Test.ProbabilityCheck
       ( TestableDistribution
       , SampleableDistribution
       , testViaWilcoxMatchedPair
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
--an error if alpha is < 2^-1023. Also alpha should be restricted to
--0-1.
testViaWilcoxMatchedPair :: Double -> Gen (Double, Double) -> TestTree
testViaWilcoxMatchedPair alpha genPair =
  testCase "WilcoxMatchedPair test"
  (do
      samples <- generate $ vectorOf sampleSize genPair
      -- This test will be more clear. Nothing should never
      -- happen. (Just Insignificant) means the tested pairs were not
      -- within the desired probability.
      (Just Significant) @=? (wilcoxonMatchedPairTest TwoTailed alpha (UV.fromList $ map fst samples) (UV.fromList $ map snd samples)))
    where sampleSize = ceiling $ logBase 2 (1/alpha)

-- Obviously inefficient can be replaced.
smallestCentralBinomialCoefficientGreaterThan :: Double -> Int
smallestCentralBinomialCoefficientGreaterThan n = helper 1 n
  where helper a n = if ((2*a) `choose` a) > n then a else helper (a+1) n

-- Note this assumes that all the approximates have the same confidence.
testSameConfidenceApproximates :: (Ord a) => Double -> Gen ((Approximate a), a) -> TestTree
testSameConfidenceApproximates p genApprox =
  testCase "MannWhitneyU test on Approximates"
  (do
      (conf, actuals) <- ((generate $ sequence $ take sampleSize $ repeat genApprox) >>= actualsToFirstConfAndActuals)
      expectedBools <- generate $ vectorOf sampleSize $
                        frequency [(fromIntegral $ numerator conf, return True)
                                  , (fromIntegral $ (denominator conf) - (numerator conf), return False)]
      (Just Significant) @=? (mannWhitneyUtest OneTailed p (boolsToSample expectedBools) (pairsToSample actuals))
  )
    where sampleSize = smallestCentralBinomialCoefficientGreaterThan (1/p)
          getConfidence ((Approximate c _ _ _):_) = toRational c
          boolsToSample = UV.fromList . (map (fromIntegral . fromEnum))
          pairToBool ((Approximate _ lo _ hi), a) = a >= lo && a <= hi
          pairsToSample = boolsToSample . map pairToBool
          actualsToFirstConfAndActuals :: [(Approximate a, a)] -> IO (Rational, [(Approximate a, a)])
          actualsToFirstConfAndActuals lst@(((Approximate c _ _ _),_):_) = return (toRational c, lst)



