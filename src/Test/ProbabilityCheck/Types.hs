{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Test.ProbabilityCheck.Types
       ( DistributionTestResult(..)
       , DistributionTestValue(..)
       , StreamStdDev(..), ssdStdDev
       , initSSD, updateSSD, ssdConduit
       , EBSState(..)
       , Action(..)
       , Args(..), stdArgs, State(..)
       , ProbTestable(..)
       , FoobarResult(..)
       ) where

import Test.QuickCheck.Property (succeeded, failed, rejected, Result(reason))
import Test.QuickCheck (Testable(..), Arbitrary(..))
import Data.Conduit (Conduit, await, yield, (=$=))
import qualified Data.Conduit.List as CL
import Control.Monad (void)
import Test.QuickCheck.Random (QCGen)
import Test.QuickCheck.Text (Terminal)
import Data.Ratio ((%))
import Test.QuickCheck.Gen (Gen(..))

-- This probably wants better naming at some point.
data DistributionTestResult a = DistributionTestResult
                                { dtrValue :: DistributionTestValue
                                , dtrTestedMean :: a
                                , dtrStdDev :: a
                                , dtrSampleSize :: Integer
                                , dtrUpperBound :: a
                                , dtrLowerBound :: a
                                }
                              deriving (Show, Eq) 

instance Testable (DistributionTestResult a) where
  property dtr = property $ case dtrValue dtr of
    TestZero -> succeeded
    TestNegative -> failed {reason = "Tested mean less than zero."}
    TestPositive -> failed {reason = "Tested mean greater than zero."}
    TestInsufficientSample -> rejected {reason = "Insufficient Samples available."}
  exhaustive _ = True

data DistributionTestValue = TestZero
                           | TestNegative
                           | TestPositive
                           | TestInsufficientSample
                           deriving (Show, Eq, Ord)

-- StreamStdDev and its basic functions probably want to be moved to
-- another file.
data StreamStdDev a = StreamStdDev
    { ssdCount :: !Integer
    , ssdMean :: !a
    , ssdS :: !a
    }
    deriving (Eq)

instance (Show a, Floating a) => Show (StreamStdDev a) where
  show ssd@(StreamStdDev count mean s) =
    "StreamStdDev {ssdCount = " ++ (show count)
    ++ ", ssdMean = " ++ (show mean)
    ++ ", ssdStdDev = " ++ (show $ ssdStdDev ssd)
    ++ ", ssdS = " ++ (show s)
    ++ "}"

ssdStdDev :: (Floating a) => StreamStdDev a -> a
ssdStdDev ssd = sqrt ((ssdS ssd) / ((fromIntegral $ ssdCount ssd) - 1))

initSSD :: (Num a) => a -> StreamStdDev a
initSSD x = StreamStdDev 1 x 0

updateSSD :: (Fractional a) => a -> StreamStdDev a -> StreamStdDev a
updateSSD x (StreamStdDev prevC prevM prevS) = StreamStdDev {ssdCount = newC, ssdMean = newM, ssdS = newS}
    where newC = prevC + 1
          newM = prevM + (x-prevM)/(fromIntegral newC)
          newS = prevS + (x-prevM)*(x-newM)

ssdConduit :: (Fractional a, Monad m) => Conduit a m (StreamStdDev a)
ssdConduit = do
  mFirst <- await
  case mFirst of
    Nothing -> return ()
    Just first -> do
      yield (initSSD first)
      void (CL.mapAccum updateSSDPair $ initSSD first) =$= CL.map fst
        where updateSSDPair a s = (newSSD, (newSSD, a))
                where newSSD = updateSSD a s

data EBSState a = EBSState
    { ebsSSD :: StreamStdDev a
    , ebsCt :: a
    , ebsT :: Integer
    , ebsK :: Integer
    , ebsX :: a
    , ebsDk :: a
    , ebsAlpha :: Rational
    } deriving (Show)

data Action -- Place holder for potentially more complicated or less complicated system.
  = Pass
  | Warn
  | Fail

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

data FoobarResult = FoobarResult



