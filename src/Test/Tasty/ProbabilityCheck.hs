{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Test.Tasty.ProbabilistyCheck
       ( testProbabilistic
       ) where

import Test.Tasty (TestName, TestTree)
import Data.Number.Erf (InvErf)

import Test.ProbabilityCheck

testProbabilistic :: ProbTestable a => TestName -> a -> TestTree
testProbabilistic testName testable = undefined

class ProbTestable prob where
  ptSample :: (InvErf a, RealFrac a, Ord a, Show a, MonadIO m) => prob -> Source m a
  ptAlpha :: (InvErf a, RealFrac a, Ord a, Show a) => prob -> a
  ptMinDiff :: (InvErf a, RealFrac a, Ord a, Show a) => prob -> MinDiff a

-- There wants to be a basic Data type similar to QuickCheck's Result
-- which other instances of this use. However it'll need to have a
-- numeric type due to the non-binary nature of the results.

-- Eventually want to add in proper error handling similar to
-- QuickCheck's RoseResult though for the moment skipping.
