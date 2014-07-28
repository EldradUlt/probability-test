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
import Data.Conduit (($$))
import qualified Data.Conduit.List as CL
import System.Random.MWC (create)
import System.Random.MWC.Distributions (normal)

main :: IO ()
main =
  defaultMain $
  testGroup "probability-test's Tests"
  [ testGroup "Tests for "
    [ testProperty "Correct Approximates claiming 100% accuracy never fails test." $ do
         testResult <- testSameConfidenceApproximates 0.05 $ genApproximate 1 1
         case testResult of
           Just NotSignificant -> return succeeded
           Just Significant -> return $ failed {reason = "\nSignificant difference between Approximate's confidence and actual success rate."}
           Nothing -> return $ failed {reason = "Sample size too small."}
    , testProperty "Incorrect Approximates claiming 100% accuracy always fail test." $ do
         testResult <- testSameConfidenceApproximates 0.05 $ genApproximate 1 0
         case testResult of
           Just NotSignificant -> return $ failed {reason = "\nNo significant difference between Approximate's confidence and actual success rate."}
           Just Significant -> return succeeded
           Nothing -> return $ failed {reason = "Sample size too small."}
    , testCase "Low Type I error" $ ((Just NotSignificant) @=?) =<< (generate $ testSameConfidenceApproximates 0.05 (genApproximate 0.5 0.5))
    , testCase "Low Type II error" $ ((Just Significant) @=?) =<< (generate $ testSameConfidenceApproximates 0.05 (genApproximate 0.75 0.25))
    ]
  , testGroup "Tests for testApproximates"
    [ testProperty "Correct Approximates with random claimed accuracies never fails test." $ do
         testResult <- testApproximates 0.05 $ genVerriedApproximate 1
         case testResult of
           Just NotSignificant -> return succeeded
           Just Significant -> return $ failed {reason = "\nSignificant difference between Approximate's confidence and actual success rate."}
           Nothing -> return $ failed {reason = "Sample size too small."}
    , testProperty "Incorrect Approximates with random claimed accuracies always fails test." $ do
         testResult <- testApproximates 0.05 $ genVerriedApproximate (-1)
         case testResult of
           Just NotSignificant -> return $ failed {reason = "\nNo significant difference between Approximate's confidence and actual success rate."}
           Just Significant -> return succeeded
           Nothing -> return $ failed {reason = "Sample size too small."}
    , testCase "Consistantly underconfident Approximates should pass a test." $
      ((Just NotSignificant) @=?) =<< (generate $ testApproximates 0.05 $ genVerriedApproximate 0.1)
    , testCase "Consistantly overconfident Approximates should fail a test." $
      ((Just Significant) @=?) =<< (generate $ testApproximates 0.05 $ genVerriedApproximate (-0.1))
    , testCase "Consistantly accurately confident Approximates shoudl pass a test." $
      ((Just NotSignificant) @=?) =<< (generate $ testApproximates 0.05 $ genVerriedApproximate 0)
    ]
  , testGroup "Tests for testNormDistSink"
    [ testCase "Passing simple case" $ (Just TestSame) @=? (zeroSource $$ testNormDistSink 0.05 0.05 0.01)
    , testCase "Failing simple case" $ (Just TestGreater) @=? (oneOnehundrethSource $$ testNormDistSink 0.05 0.05 0.01)
    , testCase "Passing self test" $ (Just TestSame) @=?
               ( (CL.unfoldM (\_ -> do
                                passed <- (Just TestSame) == (zeroSource $$ testNormDistSink 0.05 0.05 0.01)
                                return (if passed then 0 else 1)) ())
                 $$ testNormDistSink 0.05 0.05 1)
    , testCase "Failing self test" $ (Just TestSame) @=?
               ( (CL.unfoldM (\_ -> do
                                passed <- (Just TestGreater) == (oneOnehundrethSource $$ testNormDistSink 0.05 0.05 0.01)
                                return (if passed then 0 else 1)) ())
                 $$ testNormDistSink 0.05 0.05 1)
    ]
  ]

normalDoubleSource :: Double -> Source IO Double
normalDoubleSource mean = CL.unfoldM (\_ -> do
                                        gen <- create
                                        normal mean 1 gen) ()

zeroSource :: Source IO Double
zeroSource = normalDoubleSource 0

oneOnehundrethSource :: Source IO Double
oneOnehundrethSource = normalDoubleSource 0.01

genApproximate :: Log Double -> Double -> Gen (Approximate Integer, Integer)
genApproximate assertedConf actualAccuracy = do
  (low, mid, high) <- (arbitrary :: Gen (Integer, Integer, Integer))
                      >>= (\(a,b,c) -> return (min a $ min b c, min (max a b) $ min (max b c) (max a c), max a $ max b c))
  accurate <- choose (0,1) >>= (\a -> return $ a <= actualAccuracy)
  return $ (Approximate assertedConf low mid high, if accurate && (not $ actualAccuracy == 0) then mid else low - 1)

genVerriedApproximate :: Double -> Gen (Approximate Integer, Integer)
genVerriedApproximate bias = do
  r <- choose (0,1) :: Gen Double
  genApproximate (fromRational $ toRational r) (r+bias)



