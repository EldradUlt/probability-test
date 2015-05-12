{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Test.TestApproximate
       ( testApproximate
       , testApproximateWith
       , testApproximateResult
       , testApproximateWithResult
       , ApproxTestArgs(..), stdArgs
       , ApproxTestResult(..)
       ) where

import Test.QuickCheck (Arbitrary(..), Gen, generate, resize)
import Data.Approximate (Approximate(..))
import Data.Conduit (Source, ($$))
import qualified Data.Conduit.List as CL
import Numeric.Log.Signed (SignedLog)
import Test.ProbabilityCheck.Types (DistributionTestResult(..), DistributionTestValue(..))
import Test.ProbabilityCheck.EBS(empiricalBernstienStopping)

data ApproxTestArgs = ApproxTestArgs
                      { ataDelta :: SignedLog Double
                      , ataEpsilon :: SignedLog Double }

newtype ApproxTestResult = ApproxTestResult {atrResult :: DistributionTestResult (SignedLog Double)}

stdArgs :: ApproxTestArgs
stdArgs = ApproxTestArgs {ataDelta = 0.05, ataEpsilon = 0.01}

testApproximate :: (Arbitrary a, Ord b) => (a -> Approximate b) -> (a -> b) -> IO ()
testApproximate = testApproximateWith stdArgs

testApproximateWith :: (Arbitrary a, Ord b) => ApproxTestArgs -> (a -> Approximate b) -> (a -> b) -> IO ()
testApproximateWith args cApp cAct = testApproximateWithResult args cApp cAct >> return ()

testApproximateResult :: (Arbitrary a, Ord b) => (a -> Approximate b) -> (a -> b) -> IO ApproxTestResult
testApproximateResult = testApproximateWithResult stdArgs

testApproximateWithResult :: (Arbitrary a, Ord b) => ApproxTestArgs -> (a -> Approximate b) -> (a -> b) -> IO ApproxTestResult
testApproximateWithResult args cApp cAct = do
  let delta = ataDelta args
      epsilon = ataEpsilon args
      sample = arbitrary
      value :: Gen (SignedLog Double)
      value = sample >>= sampleToValue
      sampleToValue a = let (Approximate conf lo _ hi) = cApp a
                            act = cAct a
                            diff = (if lo <= act && act <= hi then 1 else 0) - (realToFrac conf)
                        in return diff
  r <- (monadicToSource $ generate $ resize 1000 value) $$ empiricalBernstienStopping 2 delta epsilon
  case dtrValue r of
    TestZero -> print "Confidence is accurate."
    TestPositive -> print "Confidence is lower than actual accuracy."
    TestNegative -> print "Confidence is incorrectly high."
    TestInsufficientSample -> print "Unable to generate sufficient samples. This should not be possible."
  return $ ApproxTestResult r

-- Helper function which turns a monadic value into a
-- Data.Conduit.Source of those values.
monadicToSource :: (Monad m) => m a -> Source m a
monadicToSource ma = CL.unfoldM (\_ -> ma >>= (\a -> return $ Just (a,()))) ()
