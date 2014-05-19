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
       testResult <- testSameConfidenceApproximates 0.05 $ do
         (high,mid,low) <- (arbitrary :: Gen (Integer, Integer, Integer))
                           >>= (\(a,b,c) -> return (max a $ max b c, min (max a b) $ min (max b c) (max a c), min a $ min b c))
         return (Approximate 1 low mid high, mid)
       case testResult of
         Just NotSignificant -> return succeeded
         Just Significant -> return $ failed {reason = "\nSignificant difference between Approximate's confidence and actual success rate."}
         Nothing -> return $ failed {reason = "Sample size too small."}
  , testProperty "Incorrect Approximates claiming 100% accuracy always fail test." $ do
       testResult <- testSameConfidenceApproximates 0.05 $ do
         (high,mid,low) <- (arbitrary :: Gen (Integer, Integer, Integer))
                           >>= (\(a,b,c) -> return (max a $ max b c, min (max a b) $ min (max b c) (max a c), min a $ min b c))
         return (Approximate 1 low mid high, low-1)
       case testResult of
         Just NotSignificant -> return $ failed {reason = "\nNo significant difference between Approximate's confidence and actual success rate."}
         Just Significant -> return succeeded
         Nothing -> return $ failed {reason = "Sample size too small."}
  , testCase "Low Type I error" $ ((Just NotSignificant) @=?) =<< (generate $ testSameConfidenceApproximates 0.05 (genApproximate 0.5 0.5))
  , testCase "Low Type II error" $ ((Just Significant) @=?) =<< (generate $ testSameConfidenceApproximates 0.05 (genApproximate 0.95 0.05))
  ]

genApproximate :: Log Double -> Double -> Gen (Approximate Integer, Integer)
genApproximate assertedConf actualAccuracy = do
  (low, mid, high) <- (arbitrary :: Gen (Integer, Integer, Integer))
                      >>= (\(a,b,c) -> return (min a $ min b c, min (max a b) $ min (max b c) (max a c), max a $ max b c))
  accurate <- choose (0,1) >>= (\a -> return $ a <= actualAccuracy)
  return $ (Approximate assertedConf low mid high, if accurate then mid else low - 1)
