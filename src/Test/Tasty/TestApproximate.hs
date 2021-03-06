{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This allows use of TestApproximate in tasty.
module Test.Tasty.TestApproximate
       ( testApproximate
       , ApproxTest(..)
       , ApproxTestDelta(..), ApproxTestEpsilon(..)
       ) where

import Test.Tasty (TestName, TestTree)
import Test.Tasty.Providers (singleTest, IsTest(..), testPassed, testFailed)
import Test.Tasty.Options (IsOption(..), OptionDescription(..), safeRead, lookupOption)
import Numeric.Log.Signed (SignedLog)
import Data.Approximate (Approximate)
import Data.Typeable (Typeable)
import Data.Proxy (Proxy(..))
import Test.QuickCheck (Arbitrary)
import Test.ProbabilityCheck.Internal (DistributionTestResult(..), DistributionTestValue(..))
import Test.TestApproximate (testApproximateWithResult, stdArgs, ApproxTestArgs(..), ApproxTestResult(..))

-- | Create a test for an Approximate.
testApproximate :: (Arbitrary a, Typeable a, Ord b, Typeable b) => TestName -> (a -> Approximate b) -> (a -> b) -> TestTree
testApproximate name cApp cAct = singleTest name $ ApproxTest {calcApproximate = cApp, calcActual = cAct}

-- | Tasty wrapper for testing an Approximate.
data ApproxTest a b = ApproxTest
                      { calcApproximate :: a -> Approximate b -- ^ Function that creates an Approximate to be tested.
                      , calcActual :: a -> b -- ^ Function which creates the corrisponding correct value.
                      } deriving Typeable

instance (Arbitrary a, Typeable a, Ord b, Typeable b) => IsTest (ApproxTest a b) where
  run opts (ApproxTest cApp cAct) _ = do
    let ApproxTestDelta delta = lookupOption opts
        ApproxTestEpsilon epsilon = lookupOption opts
    r <- testApproximateWithResult (stdArgs {ataDelta = delta, ataEpsilon = epsilon}) cApp cAct
    -- Note testApproximate should have a silent mode, which should be
    -- invoked then printing done with testPassed/Failed after running
    -- it through Test.Tasty.Runngers.formatMessage.
    return $ case dtrValue $ atrResult r of
      TestZero -> testPassed ""
      TestPositive -> testPassed ""
      TestNegative -> testFailed ""
      TestInsufficientSample -> testFailed ""
  testOptions = return
    [ Option (Proxy :: Proxy ApproxTestDelta)
    , Option (Proxy :: Proxy ApproxTestEpsilon)
    ]

-- | The allowable frequency that the test returns the incorrect result.
data ApproxTestDelta = ApproxTestDelta {getDelta :: SignedLog Double} deriving (Read, Show, Typeable)

-- | Largest value for which differences between the confidence and
-- actual accuracy should be considered the same.
data ApproxTestEpsilon = ApproxTestEpsilon {getEpsilon :: SignedLog Double} deriving (Read, Show, Typeable)

instance IsOption ApproxTestDelta where
  defaultValue = ApproxTestDelta 0.05
  parseValue = safeRead
  optionName = return "approxtest-delta"
  optionHelp = return "Acceptable error rate for test."

instance IsOption ApproxTestEpsilon where
  defaultValue = ApproxTestEpsilon 0.01
  parseValue = safeRead
  optionName = return "approxtest-epsilon"
  optionHelp = return "Minimal innaccurcy in asserted confidences to be considered sufficiently accurate."

