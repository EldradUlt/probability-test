{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Test.ProbabilityCheck
       ( TestableDistribution
       , SampleableDistribution
       , testViaWilcoxMatchedPair
       ) where

import Test.QuickCheck (Gen, generate, vectorOf)
import qualified Data.Vector.Unboxed as UV
import Statistics.Test.WilcoxonT (wilcoxonMatchedPairTest, TestType(..), TestResult(..))
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty (TestTree)

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
  testCase "WilcoxMatchedPair test" (do
    samples <- generate $ vectorOf sampleSize genPair
    -- This test will be more clear. Nothing should never
    -- happen. (Just Insignificant) means the tested pairs were not
    -- within the desired probability.
    (Just Significant) @=? (wilcoxonMatchedPairTest TwoTailed alpha (UV.fromList $ map fst samples) (UV.fromList $ map snd samples)))
    where sampleSize = ceiling $ logBase 2 (1/alpha)

