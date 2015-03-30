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
--import Data.Conduit (Conduit)

import Test.ProbabilityCheck.Types
--import Test.ProbabilityCheck.EBS

probabilityCheck :: (Testable prop) => prop -> IO ()
probabilityCheck = probabilityCheckWith stdArgs

probabilityCheckWith :: (Testable prop) => Args -> prop -> IO ()
probabilityCheckWith args p = probabilityCheckWithResult args p >> return ()

probabilityCheckWithResult :: (Testable prop) => Args -> prop -> IO FoobarResult
probabilityCheckWithResult args p = (if aChatty args then withStdioTerminal else withNullTerminal) $ \tm -> do
  rnd <- case aReplay args of
    Nothing      -> newQCGen
    Just (rnd',_) -> return rnd'
  test undefined {- some state info -} (unGen (unProperty (property' p)))
  where property' prop
          | exhaustive p = undefined -- Exhaustive cases should be handled very differently. 
          | otherwise = property prop

test :: State a -> (QCGen -> Int -> Prop) -> IO FoobarResult
test initState f = undefined



