{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, FunctionalDependencies, UndecidableInstances #-}

module Test.ProbabilityCheck.Types
       ( Action(..)
       , Args(..), stdArgs, State(..)
       , ProbTestable(..)
       , FoobarResult(..)
       ) where

import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Random (QCGen)
import Test.QuickCheck.Text (Terminal)
import Data.Ratio ((%))
import Test.QuickCheck.Gen (Gen(..))

import Test.ProbabilityCheck.Internal(DistributionTestValue(..), StreamStdDev)

data Args
  = Args
    { aReplay :: Maybe (QCGen, Int)
    , aValueAction :: DistributionTestValue -> Action -- For a given value should the test, pass, warn, and/or fail.
    , aRange :: Rational -- Absolute value of the range of possible outputs.
    , aDelta :: Rational -- Error rate.
    , aEpsilon :: Rational -- Difference from Zero to be considered Zero.
    , aChatty :: Bool -- Whether to print anything.
    }

stdArgs :: Args
stdArgs =
  Args { aReplay = Nothing
       , aValueAction = \v -> case v of
         TestZero -> Pass
         TestNegative -> Fail
         TestPositive -> Warn
         TestInsufficientSample -> Fail
       , aRange = 2
       , aDelta = 0.05
       , aEpsilon = 2 % 1000
       , aChatty = True
       }

data Action -- Place holder for potentially more complicated or less complicated system.
  = Pass
  | Warn
  | Fail

data State a
  = MkState
    { terminal                  :: Terminal          -- ^ the current terminal
    , stateRange                :: Rational          -- ^ the absolute value of the range of possible values.
    , stateDelta                :: Rational          -- ^ acceptable frequency of innacurate results.
    , stateEpsilon              :: Rational          -- ^ minimal value which is acceptably close to zero.
    , stateValueAction          :: DistributionTestValue -> Action
      
    -- dynamic ***
    , stateSSD                  :: StreamStdDev a
    , randomSeed                :: !QCGen
    }

class (Show num, RealFrac num, Floating num, Ord num) => ProbTestable prop num | prop -> num where
  genValue :: prop -> Gen num

instance ProbTestable Double Double where
  genValue a = return a

instance (Arbitrary arg, ProbTestable prop num) => ProbTestable (arg -> prop) num where
  genValue f = do
    arg <- arbitrary :: Gen arg
    let prop = f arg :: prop
    genValue prop :: Gen num

data FoobarResult = FoobarResult



