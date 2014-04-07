module Test.ProbabilityCheck
       ( TestableDistribution
       ) where

import Test.QuickCheck (Gen)

class TestableDistribution prop where
  inspect :: Gen prop


