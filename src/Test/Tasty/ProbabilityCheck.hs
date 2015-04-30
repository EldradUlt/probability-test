{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, FunctionalDependencies, DeriveDataTypeable #-}

module Test.Tasty.ProbabilityCheck
       ( testApproximate
       , ApproxTest(..)
       , ApproxTestDelta(..), ApproxTestEpsilon(..)
       ) where

import Test.Tasty (TestName, TestTree)
import Test.Tasty.Providers (singleTest, IsTest(..), testPassed, testFailed)
import Test.Tasty.Options (IsOption(..), OptionDescription(..), safeRead, lookupOption)
import Test.QuickCheck (Arbitrary(..), Gen, generate, resize)
--import Control.Monad.IO.Class (MonadIO(..))
import Data.Conduit (Source, ($$))
import qualified Data.Conduit.List as CL
import Data.Ratio (numerator, denominator)
import Numeric.Log.Signed (SignedLog)
import Data.Approximate (Approximate(..))
import Data.Typeable (Typeable)
import Data.Proxy (Proxy(..))

--import Test.ProbabilityCheck
import Test.ProbabilityCheck.EBS (empiricalBernstienStopping)
import Test.ProbabilityCheck.Types (DistributionTestResult(..), DistributionTestValue(..))

data ApproxTest a b = ApproxTest
                      { calcApproximate :: a -> Approximate b
                      , calcActual :: a -> b
                      } deriving Typeable

instance (Arbitrary a, Typeable a, Ord b, Typeable b) => IsTest (ApproxTest a b) where
  run opts (ApproxTest cApp cAct) _ = do
    let ApproxTestDelta delta = lookupOption opts
        ApproxTestEpsilon epsilon = lookupOption opts
        value :: Gen (SignedLog Double)
        value = (arbitrary :: Gen a) >>= sampleToValue
        sampleToValue :: a -> Gen (SignedLog Double)
        sampleToValue a = let (Approximate conf lo _ hi) = cApp a
                              act = cAct a
                              diff = (if lo <= act && act <= hi then 1 else 0) - (realToFrac conf)
                          in return diff
    r <- (monadicToSource $ generate $ resize 1000 value) $$ empiricalBernstienStopping 2 delta epsilon
    return $ case dtrValue r of
      TestZero -> testPassed "Confidence is accurate."
      TestPositive -> testPassed "Confidence is lower than actual accuracy."
      TestNegative -> testFailed "Confidence is incorrectly high."
      TestInsufficientSample -> testFailed "Unable to generate sufficient samples. This should not be possible."
  testOptions = return
    [ Option (Proxy :: Proxy ApproxTestDelta)
    , Option (Proxy :: Proxy ApproxTestEpsilon)
    ]

data ApproxTestDelta = ApproxTestDelta {getDelta :: SignedLog Double} deriving (Read, Show, Typeable)
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

testApproximate :: (Arbitrary a, Typeable a, Ord b, Typeable b) => TestName -> (a -> Approximate b) -> (a -> b) -> TestTree
testApproximate name cApp cAct = singleTest name $ ApproxTest {calcApproximate = cApp, calcActual = cAct}

-- Helper function which turns a monadic value into a
-- Data.Conduit.Source of those values.
monadicToSource :: (Monad m) => m a -> Source m a
monadicToSource ma = CL.unfoldM (\_ -> ma >>= (\a -> return $ Just (a,()))) ()

