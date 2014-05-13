module Main where

import Test.ProbabilityCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Data.Approximate (Approximate (..))
import Test.QuickCheck (choose, Gen)
import Test.QuickCheck.Property (failed, succeeded, Result(..))
import Statistics.Test.Types (TestResult(..))

main :: IO ()
main =
  defaultMain $
  testGroup "probability-test's Tests"
  [
    testProperty "100% Accurate Approximates never fail test." $ do
       testConfidence <- choose (0,1)
       testResult <- testSameConfidenceApproximates testConfidence $ (undefined :: Gen (Approximate Integer, Integer))
         
       case testResult of
         Just NotSignificant -> return succeeded
         Just Significant -> return $ failed {reason = "Significant difference between Approximate's confidence and actual success rate."}
         Nothing -> return $ failed {reason = "Sample size too small."}
  ]

--testSameConfidenceApproximates :: (Ord a) => Double -> Gen ((Approximate a), a) -> Gen (Maybe TestResult)

