{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Test.ProbabilityCheck
       ( probabilityCheck
       , Args(..)
       ) where

import Test.QuickCheck (Testable(..))
import Test.QuickCheck.Random (QCGen)

import Test.ProbabilityCheck.EBS

data Action -- Place holder for potentially more complicated or less complicated system.
  = Pass
  | Warn
  | Fail

data Args a
  = Args
    { replay :: Maybe (QCGen, Int)
    , valueAction :: DistributionTestValue -> Action -- For a given value should the test, pass, warn, and/or fail.
    , delta :: a -- Error rate.
    , epsilon :: a -- Difference from Zero to be considered Zero.
    , chatty :: Bool -- Whether to print anything.
    }

stdArgs :: (Fractional a) => a -> Args a
stdArgs range =
  Args { replay = Nothing
       , valueAction = \v -> case v of
         TestZero -> Pass
         TestNegative -> Fail
         TestPositive -> Warn
         TestInsufficientSample -> Fail
       , delta = 0.05
       , epsilon = range / 1000
       , chatty = True
       }

probabilityCheck :: (Testable prop, Fractional a) => a -> prop -> IO ()
probabilityCheck range = probabilityCheckWith (stdArgs range) range

probabilityCheckWith :: (Testable prop, Fractional a) => Args a -> a -> prop -> IO ()
probabilityCheckWith args range p = probabilityCheckWithResult args range p >> return ()

probabilityCheckWithResult :: (Testable prop, Fractional a) => Args a -> a -> prop -> IO (DistributionTestResult a)
probabilityCheckWithResult = undefined


