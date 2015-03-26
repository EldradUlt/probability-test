{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Test.ProbabilityCheck
       ( probabilityCheck
       , Args(..)
       ) where

import Test.QuickCheck (Testable(..), property)
import Test.QuickCheck.Gen (Gen(..))
import Test.QuickCheck.Property (Property(..), Prop(..))
import Test.QuickCheck.Random (QCGen, newQCGen)
import Test.QuickCheck.Text (withStdioTerminal, withNullTerminal)
import Data.Ratio ((%))
--import Data.Conduit (Conduit)

import Test.ProbabilityCheck.EBS

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

data State
  = MkState
    {
      -- State info
    }

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

probabilityCheck :: (Testable prop) => prop -> IO ()
probabilityCheck = probabilityCheckWith stdArgs

probabilityCheckWith :: (Testable prop) => Args -> prop -> IO ()
probabilityCheckWith args p = probabilityCheckWithResult args p >> return ()

-- Note there's a complicated relationship between the prop and the 'a' of (DistributionTestResult a) which will need to be handled.
probabilityCheckWithResult :: (Testable prop) => Args -> prop -> IO (DistributionTestResult a)
probabilityCheckWithResult args p = (if chatty args then withStdioTerminal else withNullTerminal) $ \tm -> do
  rnd <- case replay args of
    Nothing      -> newQCGen
    Just (rnd,_) -> return rnd
  test undefined {- some state info -} (unGen (unProperty (property' p)))
  where property' prop
          | exhaustive p = undefined -- Exhaustive cases should be handled very differently. 
          | otherwise = property prop

test :: State -> (QCGen -> Int -> Prop) -> IO (DistributionTestResult a)
test initState f = undefined

