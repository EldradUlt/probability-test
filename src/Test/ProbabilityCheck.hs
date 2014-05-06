{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Test.ProbabilityCheck
       ( TestableDistribution
       , SampleableDistribution
       ) where

import Test.QuickCheck (Gen)

-- this class will have methods other than inspect however inspect
-- will always be a sufficient minimal instantiation.
class TestableDistribution dist where
  distMean :: s -> Double
  distVariance :: s -> Double

class SampleableDistribution s where
  sample :: s -> Gen Double


-- N >= ((z - inverse_Phi(1 - beta)) / (mu* / sigma))^2

--wilcoxMatchedPair

