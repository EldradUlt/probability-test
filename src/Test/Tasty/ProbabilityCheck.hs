{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, FunctionalDependencies #-}

module Test.Tasty.ProbabilityCheck
       ( testProbabilistic
       , ProbTestable(..)
       , ProbabilisticTest(..)
       , normDistToProbTestable
       , pairsToProbTestable
       , approxActualToProbTestable
       , SignedLog(..)
       ) where

import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (testCase, Assertion, assertFailure)
import Test.QuickCheck (Gen, generate, frequency)
import Data.Number.Erf (InvErf(..))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Conduit (Source, ($$), ($=), transPipe)
import qualified Data.Conduit.List as CL
import Data.Maybe (fromMaybe)
import qualified Data.Sign as S
import Data.Ratio (numerator, denominator)
import Numeric.Log (Log(..), Precise)
import Data.Approximate (Approximate(..))

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

-- | Creates a ProbabilisticTest which when passed to
-- testProbabilistic tests whether the confidence of the given
-- Approximates is accurate based on the frequency with which the
-- provided Actual result falls between the lo and hi of the
-- Approximate.
--
-- /Note/: #18 This test fails if the confidences are either over or
-- under confident. The messages that the failure print out are
-- unclear.
--
-- /Bug/: #1 & #15 Test result actually has a lower confidence than the
-- value given.
--
-- /Bug/: #4 Currently produces an error if the difference between the
-- two values is the same for an entire window of the given size.
--
-- /Note/: #14 The window size should be calculated not user defined.
approxActualToProbTestable :: (InvErf a, RealFloat a, Ord a, Show a, Precise a, Integral b)
  => SignedLog a -- ^ This is 1 minus the desired confidence, which is
                 -- to say the allowable inaccuracy
                 -- rate. E.g. 0.05. (Bug #1)
  -> b -- ^ Window size to use for Wilcoxon. (Bug #4, Note #14)
  -> Gen (Approximate a, a) -- ^ Monadic sample of Approximate's and
                          -- Actual results to be tested.
  -> ProbabilisticTest Gen (SignedLog a) -- ^ Result to pass to testProbabilistic.
approxActualToProbTestable alpha size sample =
  pairsToProbTestable alpha size
  (sample >>= (\(Approximate conf lo _ hi, actual) -> do
                  let (n, d) = (numerator $ toRational conf, denominator $ toRational conf)
                      (a, b) = (fromInteger (d-n), fromInteger n)
                  c <- frequency [(a, return 0), (b, return 1)]
                  return (c, if lo <= actual && actual <= hi then 1 else 0)
                  )
   )

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

data SignedLog a = SignedLog {
  slSign :: S.Sign
  , slLn :: Log a
  }

instance (Show a, Floating a, Precise a, RealFloat a) => Show (SignedLog a) where
  show (SignedLog s a) = case s of
    S.Pos -> show a
    S.Zero -> show (0 :: Log a)
    S.Neg -> S.symbol s ++ show a

instance (Eq a) => Eq (SignedLog a) where
  (SignedLog sA a) == (SignedLog sB b) = (sA == sB) && (sA == S.Zero || a == b)
  
instance (Ord a) => Ord (SignedLog a) where
  compare (SignedLog sA a) (SignedLog sB b) = case (compare sA sB, sA) of
    (EQ, S.Pos) -> compare a b
    (EQ, S.Zero) -> EQ
    (EQ, S.Neg) -> compare b a
    (r, _) -> r

signOf :: (Num a, Ord a) => a -> S.Sign
signOf x = if x > 0 then S.Pos else if x == 0 then S.Zero else S.Neg

instance (Num a, Ord a, Precise a, RealFloat a) => Num (SignedLog a) where
  negate (SignedLog s a) = SignedLog (S.negate s) a
  abs (SignedLog s a) = SignedLog (S.abs s) a
  signum (SignedLog s _) = SignedLog s 1
  fromInteger i = SignedLog (signOf i) (fromInteger $ abs i)
  (+) (SignedLog sA a) (SignedLog sB b) =
    case (sA, sB, compare a b) of
      (S.Zero, _, _) -> SignedLog sB b
      (_, S.Zero, _) -> SignedLog sA a
      (s1, s2, _) | s1 == s2 -> SignedLog sA (a + b)
      (_, _, LT) -> SignedLog sB (b - a)
      (_, _, EQ) -> SignedLog S.Zero (a - b)
      (_, _, GT) -> SignedLog sA (a - b)
  (*) (SignedLog sA a) (SignedLog sB b) = SignedLog (S.mult sA sB) (a*b) -- This doesn't probably handle 0*NaN or 0*Inf

instance (Real a, Precise a, RealFloat a) => Real (SignedLog a) where
  toRational (SignedLog s a) = case s of
    S.Pos -> toRational a
    S.Zero -> 0
    S.Neg -> (-1) * (toRational a)

-- Um obviously lacking def.
instance (Floating a, Precise a, RealFloat a) => Floating (SignedLog a) where
  sqrt (SignedLog S.Neg _) = SignedLog S.Pos $ realToFrac $ (0/0 :: a)
  sqrt (SignedLog s a) = SignedLog s $ sqrt a
  pi = undefined
  exp = undefined
  log = undefined
  sin = undefined
  cos = undefined
  asin = undefined
  atan = undefined
  acos = undefined
  sinh = undefined
  cosh = undefined
  asinh = undefined
  atanh = undefined
  acosh = undefined

instance (Fractional a, Precise a, RealFloat a) => Fractional (SignedLog a) where
  fromRational r = case (signOf $ numerator r, signOf $ denominator r) of
    -- If sign is Zero then value is 0 reguardless of LN. Therefore if
    -- result actually wants to be some form of +/- inf or NaN the
    -- sign should not be Zero.
    (S.Zero, S.Zero) -> SignedLog S.Pos $ fromRational r
    (s, S.Zero) -> SignedLog s $ fromRational $ abs r
    (sA, sB) -> SignedLog (S.mult sA sB) $ fromRational $ abs r
  (SignedLog sA a) / (SignedLog sB b) = case sB of
    S.Neg -> SignedLog (S.negate sA) (a/b)
    _ -> SignedLog sA (a/b)

signToNum :: (Num a) => S.Sign -> a
signToNum s = case s of
  S.Pos -> 1
  S.Zero -> 0
  S.Neg -> -1

instance (RealFrac a, Precise a, RealFloat a) => RealFrac (SignedLog a) where
  properFraction (SignedLog s a) = let (i,f) = properFraction a
                                   in (i * (signToNum s), SignedLog s f)

-- This wants to be replaced with a more accurate instance.
instance (InvErf a, Precise a, RealFloat a) => InvErf (SignedLog a) where
  invnormcdf l = realToFrac $ invnormcdf (realToFrac l::a)

-- This wants to be replaced with a more accurate instance.
instance (RealFrac a, Precise a, RealFloat a) => RealFrac (Log a) where
  properFraction l = (\(b,a) -> (b, realToFrac a)) $ properFraction $ exp (ln l)

