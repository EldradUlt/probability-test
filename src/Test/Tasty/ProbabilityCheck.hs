{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Test.Tasty.ProbabilityCheck
       ( testProbabilistic
       ) where

import Test.Tasty.Providers
import Test.Tasty.Options
import qualified Test.ProbabilityCheck as PC


-- Determins wether or not the distributions a and b are the same with a confidence of c.
testProbabilistic :: (PC.TestableDistribution a x, PC.TestableDistribution b x, Ord c, Num c) => TestName -> a x -> b x -> c -> TestTree
testProbabilistic testName distA distB confidence = undefined

