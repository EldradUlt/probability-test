module Main where

import Test.ProbabilityCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (testProperty)
import Data.Approximate (Approximate (..))
import Test.QuickCheck (choose, Gen, arbitrary, generate)
import Test.QuickCheck.Property (failed, succeeded, Result(..))
import Statistics.Test.Types (TestResult(..))
import Numeric.Log

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
  , testCase "Low false Positive" $ ((Just NotSignificant) @=?) =<< (generate $ testSameConfidenceApproximates 0.999999 (genApproximate 0.5 0.5))
    -- This test fails WAY more often than they should.
  , testCase "Low false Negative" $ ((Just Significant) @=?) =<< (generate $ testSameConfidenceApproximates 0.999999999 (genApproximate 0.75 0.25))
  ]

genApproximate :: Log Double -> Double -> Gen (Approximate Integer, Integer)
genApproximate assertedConf actualAccuracy = do
  (low, mid, high) <- (arbitrary :: Gen (Integer, Integer, Integer))
                      >>= (\(a,b,c) -> return (min a $ min b c, min (max a b) $ min (max b c) (max a c), max a $ max b c))
  accurate <- choose (0,1) >>= (\a -> return $ a <= actualAccuracy)
  return $ (Approximate assertedConf low mid high, if accurate then mid else low - 1)
