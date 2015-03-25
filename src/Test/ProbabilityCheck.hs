{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Test.ProbabilityCheck
       ( probabilityCheck
       ) where

import Test.QuickCheck (Testable(..))

import Test.ProbabilityCheck.EBS

probabilityCheck :: (Testable prop) => prop -> IO ()
probabilityCheck = undefined
