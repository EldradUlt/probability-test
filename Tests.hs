module Main where

import Test.ProbabilityCheck
import Test.Tasty
--import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Data.Approximate (Approximate (..))
import Test.QuickCheck (choose, Gen, arbitrary)
import Test.QuickCheck.Property (failed, succeeded, Result(..))
import Statistics.Test.Types (TestResult(..))

main :: IO ()
main =
  defaultMain $
  testGroup "probability-test's Tests"
  [
    testProperty "Correct Approximates claiming 100% accuracy never fail test." $ do
       testConfidence <- choose (0,1) >>= (return . (1-))
       testResult <- testSameConfidenceApproximates testConfidence $ do
         (high,mid,low) <- (arbitrary :: Gen (Integer, Integer, Integer))
                           >>= (\(a,b,c) -> return (max a $ max b c, min (max a b) $ min (max b c) (max a c), min a $ min b c))
         return (Approximate 1 low mid high, mid)
       case testResult of
         Just NotSignificant -> return succeeded
         Just Significant -> return $ failed {reason = "Significant difference between Approximate's confidence and actual success rate."}
         Nothing -> return $ failed {reason = "Sample size too small."}
  , testProperty "Incorrect Approximates claiming 100% accuracy always fail test." $ do
       testConfidence <- choose (0,1) >>= (return . (1-))
       testResult <- testSameConfidenceApproximates testConfidence $ do
         (high,mid,low) <- (arbitrary :: Gen (Integer, Integer, Integer))
                           >>= (\(a,b,c) -> return (max a $ max b c, min (max a b) $ min (max b c) (max a c), min a $ min b c))
         return (Approximate 1 low mid high, low-1)
       case testResult of
         Just NotSignificant -> return $ failed {reason = "No significant difference between Approximate's confidence and actual success rate."}
         Just Significant -> return succeeded
         Nothing -> return $ failed {reason = "Sample size too small."}
  ]

--testSameConfidenceApproximates :: (Ord a) => Double -> Gen ((Approximate a), a) -> Gen (Maybe TestResult)

