{-# LANGUAGE FlexibleContexts #-}

module Test.ProbabilityCheck
       ( TestableDistribution
       ) where

import Test.QuickCheck (Gen)

class TestableDistribution (dist a) where
  inspect :: dist a -> Gen a


instance TestableDistribution (Gen a) where
  inspect = id


