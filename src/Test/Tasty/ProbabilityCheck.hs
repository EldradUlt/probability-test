{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, FunctionalDependencies #-}

module Test.Tasty.ProbabilityCheck
       ( testProbabilistic
       , ProbTestable(..)
       , ProbabilisticTest(..)
       , normDistToProbTestable
       , pairsToProbTestable
       ) where

import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (testCase, Assertion, assertFailure)
import Test.QuickCheck (Gen, generate)
import Data.Number.Erf (InvErf)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Conduit (Source, ($$), ($=), transPipe)
import qualified Data.Conduit.List as CL
import Data.Maybe (fromMaybe)

import Test.ProbabilityCheck

-- | Creates a ProbabilisticTest for testing whether the given sample
-- is a Normal Distribution with a mean of zero.
--
-- /Bug/: #1 Test result actually has a lower confidence and a larger
-- minimal difference than the values given.
normDistToProbTestable ::
  (InvErf a, RealFrac a, Ord a, Show a, Monad m)
  => a -- ^ Desired confidence of test's accuracy. (Bug #1)
  -> a -- ^ Desired minimal difference from zero for mean to be
       -- considered not zero. (Bug #1)
  -> m a -- ^ Monadic sample to be tested. Should be indepent.
  -> ProbabilisticTest m a -- ^ Result to pass to testProbabilistic.
normDistToProbTestable conf minDiff sample = ProbabilisticTest
  { ptS = monadicToSource sample
  , ptA = conf
  , ptMD = MDAbsolute minDiff
  , ptNF = valueHighMessage
  , ptPF = valueLowMessage
  }

-- Test true if for each pair the values come from distributions with
-- the same mean. Note the distributions are not contrained in any way
-- and do not need to be the same between pairs.
pairsToProbTestable :: (InvErf a, RealFrac a, Ord a, Show a, Integral b, Monad m) => a -> b -> m (a,a) -> ProbabilisticTest m a
pairsToProbTestable conf size sample = ProbabilisticTest
  { ptS = (monadicToSource sample) $= wilcoxonRankedPairsConduit (fromIntegral size)
  , ptA = conf
  , ptMD = MDRelative 0.15 --This value is hardcoded for the moment. But should be calculated from the Confidence.
  , ptNF = valueHighMessage
  , ptPF = valueLowMessage
  }

valueHighMessage :: (Show a) => DistributionTestResult a -> Maybe String
valueHighMessage dtr = Just $ "Actual tested value was less than expected value.\n" ++ show dtr

valueLowMessage :: (Show a) => DistributionTestResult a -> Maybe String
valueLowMessage dtr = Just $ "Actual tested value was less than expected value.\n" ++ show dtr

testProbabilistic :: (ProbTestable p m a, InvErf a, RealFrac a, Ord a, Show a, MonadIO m) => TestName -> p -> TestTree
testProbabilistic testName p = testCase testName $ (toAssertion p) $ (ptSample p) $$ testNormDistSink True (ptAlpha p) (ptMinDiff p)

toAssertion :: (ProbTestable p m a, Show a) => p -> m (DistributionTestResult a) -> Assertion
toAssertion p resIO = do
  res <- ptToIO p resIO
  case dtrValue res of
    TestZero -> return ()
    TestNegative -> fromMaybe (return ()) ((ptNegFail p res) >>= (return . assertFailure))
    TestPositive -> fromMaybe (return ()) ((ptPosFail p res) >>= (return . assertFailure))
    TestInsufficientSample -> assertFailure $ "Test unable to get sufficient samples.\n" ++ (show res)

class ProbTestable prob m a | prob -> m a where
  ptSample :: (InvErf a, RealFrac a, Ord a, Show a) => prob -> Source m a
  ptAlpha :: (InvErf a, RealFrac a, Ord a, Show a) => prob -> a
  ptMinDiff :: (InvErf a, RealFrac a, Ord a, Show a) => prob -> MinDiff a
  ptNegFail :: prob -> DistributionTestResult a -> Maybe String
  ptPosFail :: prob -> DistributionTestResult a -> Maybe String
  ptToIO :: prob -> m x -> IO x

data ProbabilisticTest m a =
  ProbabilisticTest
  { ptS :: Source m a
  , ptA :: a
  , ptMD :: MinDiff a
  , ptNF :: DistributionTestResult a -> Maybe String
  , ptPF :: DistributionTestResult a -> Maybe String
  }

monadicToSource :: (Monad m) => m a -> Source m a
monadicToSource ma = CL.unfoldM (\_ -> ma >>= (\a -> return $ Just (a,()))) ()

instance (InvErf a, RealFrac a, Ord a, Show a) => ProbTestable (ProbabilisticTest IO a) IO a where
  ptSample p = ptS p
  ptAlpha = ptA
  ptMinDiff = ptMD
  ptNegFail = ptNF
  ptPosFail = ptPF
  ptToIO _ = id

instance (InvErf a, RealFrac a, Ord a, Show a) => ProbTestable (ProbabilisticTest Gen a) IO a where
  ptSample p = transPipe generate $ ptS p
  ptAlpha = ptA
  ptMinDiff = ptMD
  ptNegFail = ptNF
  ptPosFail = ptPF
  ptToIO _ = id

-- There wants to be a basic Data type similar to QuickCheck's Result
-- which other instances of this use. However it'll need to have a
-- numeric type due to the non-binary nature of the results.

-- Eventually want to add in proper error handling similar to
-- QuickCheck's RoseResult though for the moment skipping.
