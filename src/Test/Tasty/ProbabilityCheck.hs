module Test.Tasty.ProbabilityCheck
       ( testProbabilistic
       ) where

import Test.Tasty.Providers
import Test.Tasty.Options
import qualified Test.ProbabilityCheck as PC


-- Determins wether or not the distributions a and b are the same with a confidence of c.
testProbabilistic :: (PC.TestableDistribution a, PC.TestableDistribution b, Ord c, Num c) => TestName -> a -> b -> c -> TestTree
testProbabilistic testName distA distB confidence = undefined

