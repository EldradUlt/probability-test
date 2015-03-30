{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

module Test.ProbabilityCheck.Types
       ( DistributionTestResult(..)
       , DistributionTestValue(..)
       , StreamStdDev(..), ssdStdDev
       , initSSD, updateSSD, ssdConduit
       , EBSState(..)
       ) where

import Test.QuickCheck.Property (succeeded, failed, rejected, Result(reason))
import Test.QuickCheck (Testable(..))
import Data.Conduit (Conduit, await, yield, (=$=))
import qualified Data.Conduit.List as CL
import Control.Monad (void)

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


