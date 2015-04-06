{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Test.ProbabilityCheck
       ( probabilityCheck
       , Args(..)
       , State(..)
       , FoobarResult(..)
       ) where

import Test.QuickCheck (Testable(..), property)
import Test.QuickCheck.Gen (Gen(..))
import Test.QuickCheck.Property (Property(..), Prop(..))
import Test.QuickCheck.Random (QCGen, newQCGen)
import Test.QuickCheck.Text (withStdioTerminal, withNullTerminal)
import Data.Conduit (Conduit)

import Test.ProbabilityCheck.Types
--import Test.ProbabilityCheck.EBS

probabilityCheck :: (ProbTestable prop num) => prop -> IO ()
probabilityCheck = probabilityCheckWith stdArgs

probabilityCheckWith :: (ProbTestable prop num) => Args -> prop -> IO ()
probabilityCheckWith args p = probabilityCheckWithResult args p >> return ()

probabilityCheckWithResult :: (ProbTestable prop num) => Args -> prop -> IO (DistributionTestResult num)
probabilityCheckWithResult args p = (if aChatty args then withStdioTerminal else withNullTerminal) $ \tm -> do
  rnd <- case aReplay args of
    Nothing      -> newQCGen
    Just (rnd',_) -> return rnd'
  test {-MkState {
    terminal                  :: Terminal          -- ^ the current terminal
    , stateRange                :: Rational          -- ^ the absolute value of the range of possible values.
    , stateDelta                :: Rational          -- ^ acceptable frequency of innacurate results.
    , stateEpsilon              :: Rational          -- ^ minimal value which is acceptably close to zero.
    , stateValueAction          :: DistributionTestValue -> Action
                                   
                                   -- dynamic ***
    , stateSSD                  :: StreamStdDev a
    , randomSeed           
    } -} undefined
    (unGen (genValue p))
    --(unGen (unProperty (property' p)))
    
-- empiricalBernstienStopping :: (Show a, RealFrac a, Floating a, Ord a, MonadIO m) => a -> a -> a -> Sink a m (DistributionTestResult a)

-- Needs changing.
test :: State num -> (QCGen -> Int -> num) -> IO (DistributionTestResult num)
test s f = undefined
  -- do
  -- $$ empiricalBernstienStopping (stateRange s) (stateDelta s) (stateEpsilon s)



