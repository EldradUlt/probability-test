{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Test.ProbabilityCheck
       ( probabilityCheck
       , Args(..)
       , State(..)
       , FoobarResult(..)
       ) where

import Test.QuickCheck (Testable(..), property, Arbitrary(..))
import Test.QuickCheck.Gen (Gen(..))
import Test.QuickCheck.Property (Property(..), Prop(..))
import Test.QuickCheck.Random (QCGen, newQCGen)
import Test.QuickCheck.Text (withStdioTerminal, withNullTerminal, Terminal)
import Data.Ratio ((%))
--import Data.Conduit (Conduit)

import Test.ProbabilityCheck.Types
--import Test.ProbabilityCheck.EBS

data Action -- Place holder for potentially more complicated or less complicated system.
  = Pass
  | Warn
  | Fail

data Args
  = Args
    { replay :: Maybe (QCGen, Int)
    , valueAction :: DistributionTestValue -> Action -- For a given value should the test, pass, warn, and/or fail.
    , range :: Rational -- Absolute value of the range of possible outputs.
    , delta :: Rational -- Error rate.
    , epsilon :: Rational -- Difference from Zero to be considered Zero.
    , chatty :: Bool -- Whether to print anything.
    }

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

class ProbTestable prop where
  genValue :: (Show a, RealFrac a, Floating a, Ord a) => prop -> Gen a

{-
instance (Show a, RealFrac a, Floating a, Ord a) => ProbTestable a where
  genValue a = return a
-}

instance (Arbitrary arg, ProbTestable prop) => ProbTestable (arg -> prop) where
  genValue f = arbitrary >>= genValue . f

stdArgs :: Args
stdArgs =
  Args { replay = Nothing
       , valueAction = \v -> case v of
         TestZero -> Pass
         TestNegative -> Fail
         TestPositive -> Warn
         TestInsufficientSample -> Fail
       , range = 2
       , delta = 0.05
       , epsilon = 2 % 1000
       , chatty = True
       }

data FoobarResult = FoobarResult

probabilityCheck :: (Testable prop) => prop -> IO ()
probabilityCheck = probabilityCheckWith stdArgs

probabilityCheckWith :: (Testable prop) => Args -> prop -> IO ()
probabilityCheckWith args p = probabilityCheckWithResult args p >> return ()

probabilityCheckWithResult :: (Testable prop) => Args -> prop -> IO FoobarResult
probabilityCheckWithResult args p = (if chatty args then withStdioTerminal else withNullTerminal) $ \tm -> do
  rnd <- case replay args of
    Nothing      -> newQCGen
    Just (rnd',_) -> return rnd'
  test undefined {- some state info -} (unGen (unProperty (property' p)))
  where property' prop
          | exhaustive p = undefined -- Exhaustive cases should be handled very differently. 
          | otherwise = property prop

test :: State a -> (QCGen -> Int -> Prop) -> IO FoobarResult
test initState f = undefined



