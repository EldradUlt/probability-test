{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Test.ProbabilityCheck
       ( TestableDistribution
       ) where

import Test.QuickCheck (Gen)

-- this class will have methods other than inspect however inspect
-- will always be a sufficient minimal instantiation.
class TestableDistribution dist a where
  inspect :: dist a -> Gen a

instance TestableDistribution Gen a where
  inspect = id


