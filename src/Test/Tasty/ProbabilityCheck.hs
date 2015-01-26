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

-- | Creates a ProbabilisticTest which when passed to
-- testProbabilistic tests whether the given sample is a Normal
-- Distribution with a mean of zero.
--
-- /Bug/: #1 Test result actually has a lower confidence and a larger
-- minimal difference than the values given.
normDistToProbTestable
  :: (InvErf a, RealFrac a, Ord a, Show a, Monad m)
  => a -- ^ This is 1 minus the desired confidence, which is to say
       -- the allowable inaccuracy rate. E.g. 0.05. (Bug #1)
  -> a -- ^ Desired minimal difference from zero for mean to be
       -- considered not zero. (Bug #1)
  -> m a -- ^ Monadic sample to be tested. Must produce independant values.
  -> ProbabilisticTest m a -- ^ Result to pass to testProbabilistic.
normDistToProbTestable alpha minDiff sample = ProbabilisticTest
  { ptS = monadicToSource sample
  , ptA = alpha
  , ptMD = MDAbsolute minDiff
  , ptNF = valueHighMessage
  , ptPF = valueLowMessage
  }

-- | Creates a ProbabilisticTest which when passed to
-- testProbabilistic tests whether each pair of values comes from
-- distributions with the same mean. Note the distributions are not
-- constrained in any way and do not need to be the same across
-- different pairs.
--
-- /Bug/: #1 & #15 Test result actually has a lower confidence than the
-- value given.
--
-- /Bug/: #4 Currently produces an error if the difference between the
-- two values is the same for an entire window of the given size.
--
-- /Note/: #14 The window size should be calculated not user defined.
pairsToProbTestable
  :: (InvErf a, RealFrac a, Ord a, Show a, Integral b, Monad m)
  => a -- ^ This is 1 minus the desired confidence, which is to say
       -- the allowable inaccuracy rate. E.g. 0.05. (Bug #1)
  -> b -- ^ Window size to use for Wilcoxon. (Bug #4, Note #14)
  -> m (a,a) -- ^ Monadic sample to be tested. Must produce independant values.
  -> ProbabilisticTest m a -- ^ Result to pass to testProbabilistic.
pairsToProbTestable alpha size sample = ProbabilisticTest
  { ptS = (monadicToSource sample) $= wilcoxonRankedPairsConduit (fromIntegral size)
  , ptA = alpha
  , ptMD = MDRelative 0.15 -- Bug #15
  , ptNF = valueHighMessage
  , ptPF = valueLowMessage
  }

-- Default failure message when the tested value is greater than zero. Note inappropriate for pairsToProbTestable.
valueHighMessage :: (Show a) => DistributionTestResult a -> Maybe String
valueHighMessage dtr = Just $ "Actual tested value was less than expected value.\n" ++ show dtr

-- Default failure message when the tested value is less than zero. Note inappropriate for pairsToProbTestable.
valueLowMessage :: (Show a) => DistributionTestResult a -> Maybe String
valueLowMessage dtr = Just $ "Actual tested value was less than expected value.\n" ++ show dtr

-- | Create a Test for a ProbabilityCheck ProbTestable test.
testProbabilistic :: (ProbTestable p m a, InvErf a, RealFrac a, Ord a, Show a, MonadIO m)
                     => TestName -- ^ Name of test.
                     -> p -- ^ Instance of ProbTestable to use for test.
                     -> TestTree -- ^ Resulting test.
testProbabilistic testName p = testCase testName $ (toAssertion p) $ (ptSample p) $$ testNormDistSink True (ptAlpha p) (ptMinDiff p)

-- Helper function which uses the configuration values of a
-- ProbTestable to turn a DistributionTestResult into a
-- Test.HUnit.Assertion.
toAssertion :: (ProbTestable p m a, Show a) => p -> m (DistributionTestResult a) -> Assertion
toAssertion p resIO = do
  res <- ptToIO p resIO
  case dtrValue res of
    TestZero -> return ()
    TestNegative -> fromMaybe (return ()) ((ptNegFail p res) >>= (return . assertFailure))
    TestPositive -> fromMaybe (return ()) ((ptPosFail p res) >>= (return . assertFailure))
    TestInsufficientSample -> assertFailure $ "Test unable to get sufficient samples.\n" ++ (show res)

-- | Instantiations of this class contain both the sampling method to
-- be tested as well as a bunch of configuration options. This is
-- probably a mistake and should be rewritten in a more user friendly
-- fashion.
--
-- Will document more if not replaced soon.
class ProbTestable prob m a | prob -> m a where
  ptSample :: (InvErf a, RealFrac a, Ord a, Show a) => prob -> Source m a
  ptAlpha :: (InvErf a, RealFrac a, Ord a, Show a) => prob -> a
  ptMinDiff :: (InvErf a, RealFrac a, Ord a, Show a) => prob -> MinDiff a
  ptNegFail :: prob -> DistributionTestResult a -> Maybe String
  ptPosFail :: prob -> DistributionTestResult a -> Maybe String
  ptToIO :: prob -> m x -> IO x

-- | This is a data type which is a straight forward instantiation of
-- the ProbTestable class.
--
-- Will document more if not replaced soon.
data ProbabilisticTest m a =
  ProbabilisticTest
  { ptS :: Source m a
  , ptA :: a
  , ptMD :: MinDiff a
  , ptNF :: DistributionTestResult a -> Maybe String
  , ptPF :: DistributionTestResult a -> Maybe String
  }

-- Helper function which turns a monadic value into a
-- Data.Conduit.Source of those values.
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
