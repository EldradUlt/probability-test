{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Test.Tasty.ProbabilistyCheck
       ( testProbabilistic
       ) where

import Test.Tasty (TestName, TestTree)

import Test.ProbabilityCheck

testProbabilistic :: ProbTestable a => TestName -> a -> TestTree
testProbabilistic testName testable = undefined

class ProbTestable prop where
  foobar :: prop -> Bool --placeholder

