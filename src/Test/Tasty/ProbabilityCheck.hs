{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, FunctionalDependencies, DeriveDataTypeable #-}

module Test.Tasty.ProbabilityCheck
       ( SignedLog(..)
       , testApproximate
       -- , ApproxTest(..)
       , ApproxTestDelta(..), ApproxTestEpsilon(..)
       ) where

import Test.Tasty (TestName, TestTree)
import Test.Tasty.Providers (singleTest, IsTest(..), testPassed, testFailed)
import Test.Tasty.Options (IsOption(..), OptionDescription(..), safeRead, lookupOption)
import Test.QuickCheck (Arbitrary(..), Gen, generate, resize)
--import Control.Monad.IO.Class (MonadIO(..))
import Data.Conduit (Source, ($$))
import qualified Data.Conduit.List as CL
import qualified Data.Sign as S
import Data.Ratio (numerator, denominator)
import Numeric.Log (Log(..), Precise)
import Data.Approximate (Approximate(..))
import Data.Typeable (Typeable)
import Data.Proxy (Proxy(..))

--import Test.ProbabilityCheck
import Test.ProbabilityCheck.EBS (empiricalBernstienStopping)
import Test.ProbabilityCheck.Types (DistributionTestResult(..), DistributionTestValue(..))

data ApproxTest a b = ApproxTest
                      { calcApproximate :: a -> Approximate b
                      , calcActual :: a -> b
                      } deriving Typeable

instance (Arbitrary a, Typeable a, Ord b, Typeable b) => IsTest (ApproxTest a b) where
  run opts (ApproxTest cApp cAct) _ = do
    let ApproxTestDelta delta = lookupOption opts
        ApproxTestEpsilon epsilon = lookupOption opts
        value :: Gen (SignedLog Double)
        value = (arbitrary :: Gen a) >>= sampleToValue
        sampleToValue :: a -> Gen (SignedLog Double)
        sampleToValue a = let (Approximate conf lo _ hi) = cApp a
                              act = cAct a
                              diff = (if lo <= act && act <= hi then 1 else 0) - (SignedLog S.Pos conf)
                          in return diff
    r <- (monadicToSource $ generate $ resize 1000 value) $$ empiricalBernstienStopping 2 delta epsilon
    return $ case dtrValue r of
      TestZero -> testPassed "Confidence is accurate."
      TestPositive -> testPassed "Confidence is lower than actual accuracy."
      TestNegative -> testFailed "Confidence is incorrectly high."
      TestInsufficientSample -> testFailed "Unable to generate sufficient samples. This should not be possible."
  testOptions = return
    [ Option (Proxy :: Proxy ApproxTestDelta)
    , Option (Proxy :: Proxy ApproxTestEpsilon)
    ]

data ApproxTestDelta = ApproxTestDelta {getDelta :: SignedLog Double} deriving (Read, Show, Typeable)
data ApproxTestEpsilon = ApproxTestEpsilon {getEpsilon :: SignedLog Double} deriving (Read, Show, Typeable)

instance IsOption ApproxTestDelta where
  defaultValue = ApproxTestDelta 0.05
  parseValue = safeRead
  optionName = return "approxtest-delta"
  optionHelp = return "Acceptable error rate for test."

instance IsOption ApproxTestEpsilon where
  defaultValue = ApproxTestEpsilon 0.01
  parseValue = safeRead
  optionName = return "approxtest-epsilon"
  optionHelp = return "Minimal innaccurcy in asserted confidences to be considered sufficiently accurate."

testApproximate :: (Arbitrary a, Typeable a, Ord b, Typeable b) => TestName -> (a -> Approximate b) -> (a -> b) -> TestTree
testApproximate name cApp cAct = singleTest name $ ApproxTest {calcApproximate = cApp, calcActual = cAct}

{-
-- Assumes conf between 0 and 1.
testApproximate :: (ProbTestableMonad m, Ord a) => TestName -> SignedLog Double -> SignedLog Double -> m (a, Approximate a) -> TestTree
testApproximate name delta epsilon approximates =
  singleTest
  source
  $= (CL.map (\p@(_, Approximate conf _ _ _) -> (actualApproximateToOneZero p) - (fromSomethingorother conf)))
  $$ empiricalBernstienStopping 2 delta epsilon
  where source = monadicToSource approximates

actualApproximateToOneZero :: (Num a, Ord a) => (a, Approximate a) -> a
actualApproximateToOneZero (actual, Approximate _ lo _ hi) = if lo <= actual && actual <= hi then 1 else 0
-}

-- Helper function which turns a monadic value into a
-- Data.Conduit.Source of those values.
monadicToSource :: (Monad m) => m a -> Source m a
monadicToSource ma = CL.unfoldM (\_ -> ma >>= (\a -> return $ Just (a,()))) ()

data SignedLog a = SignedLog {
  slSign :: S.Sign
  , slLn :: Log a
  } deriving (Show, Read)

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
  sqrt (SignedLog S.Neg _) = SignedLog S.Pos $ Exp (0/0)
  sqrt (SignedLog s a) = SignedLog s $ sqrt a
  pi = error "Todo, define: pi"
  exp = error "Todo, define: exp"
  log (SignedLog S.Neg _) = SignedLog S.Pos $ Exp (0/0)
  log (SignedLog S.Zero _) = SignedLog S.Neg $ Exp (1/0)
  log (SignedLog S.Pos (Exp a)) | a >  0 = SignedLog S.Pos $ log $ Exp a
  log (SignedLog S.Pos (Exp a)) | a == 0 = SignedLog S.Zero $ Exp (-1/0)
  log (SignedLog S.Pos (Exp a)) | a <  0 = SignedLog S.Neg $ log $ Exp $ abs a
  log (SignedLog S.Pos _) = SignedLog S.Pos $ Exp (0/0)
  sin = error "Todo, define: sin"
  cos = error "Todo, define: cos"
  asin = error "Todo, define: asin"
  atan = error "Todo, define: atan"
  acos = error "Todo, define: acos"
  sinh = error "Todo, define: sinh"
  cosh = error "Todo, define: cosh"
  asinh = error "Todo, define: asinh"
  atanh = error "Todo, define: atanh"
  acosh = error "Todo, define: acosh"

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
instance (RealFrac a, Precise a, RealFloat a) => RealFrac (Log a) where
  properFraction l = (\(b,a) -> (b, realToFrac a)) $ properFraction $ exp (ln l)

