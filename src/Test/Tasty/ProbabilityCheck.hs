{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, FunctionalDependencies #-}

module Test.Tasty.ProbabilityCheck
       ( testProbabilistic
       , ProbTestable(..)
       , ProbabilisticTest(..)
       ) where

import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (testCase, Assertion)
import Data.Number.Erf (InvErf)
import Control.Monad.IO.Class (MonadIO)
import Data.Conduit (Source, ($$))

import Test.ProbabilityCheck

testProbabilistic :: (ProbTestable p m a, InvErf a, RealFrac a, Ord a, Show a, MonadIO m) => TestName -> p -> TestTree
testProbabilistic testName p = testCase testName $ toAssertion $ (ptSample p) $$ testNormDistSink True (ptAlpha p) (ptMinDiff p)

toAssertion :: (Show n, MonadIO m) => m (DistributionTestResult n) -> Assertion
toAssertion resIO = undefined

class ProbTestable prob m a | prob -> m a where
  ptSample :: (InvErf a, RealFrac a, Ord a, Show a, MonadIO m) => prob -> Source m a
  ptAlpha :: (InvErf a, RealFrac a, Ord a, Show a) => prob -> a
  ptMinDiff :: (InvErf a, RealFrac a, Ord a, Show a) => prob -> MinDiff a

data ProbabilisticTest m a = Foobar (m a)

instance (InvErf a, RealFrac a, Ord a, Show a, MonadIO m) => ProbTestable (ProbabilisticTest m a) m a where
  ptSample = undefined
  ptAlpha = undefined
  ptMinDiff = undefined

-- There wants to be a basic Data type similar to QuickCheck's Result
-- which other instances of this use. However it'll need to have a
-- numeric type due to the non-binary nature of the results.

-- Eventually want to add in proper error handling similar to
-- QuickCheck's RoseResult though for the moment skipping.
