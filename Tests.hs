{-# LANGUAGE TemplateHaskell, DataKinds, ScopedTypeVariables #-}
module Main where

import Test.ProbabilityCheck
import Test.Tasty
import Test.Tasty.HUnit
import Data.Conduit (($$), Source, ZipSource(..), ($=), (=$))
import qualified Data.Conduit.List as CL
import System.Random.MWC (withSystemRandom)
import System.Random.MWC.Distributions (normal)
import Data.Map.Strict (insertWith, (!))
import Control.Applicative ((<*>))
import qualified Data.HyperLogLog as HLL
import Data.Reflection (nat)
import Data.Approximate (Approximate(..))
import Data.List (nub)
import Test.QuickCheck (Gen, arbitrary, generate)
import Data.Monoid (mempty)
import Numeric.Log (Log(..), Precise)
import Data.Number.Erf (InvErf(..))
import Control.Concurrent (threadDelay)
import qualified Data.Sign as S
import Data.Ratio ((%), numerator, denominator)

main :: IO ()
main =
  defaultMain $
  testGroup "probability-test's Tests"
  [ testGroup "Tests for testNormDistSink"
    [ {-testCase "Zero simple case" $ assertResHasVal TestZero $ zeroSource $$ testNormDistSink 0.01 0.01
    , testCase "Positive simple case" $ assertResHasVal TestPositive $ oneTenthSource $$ testNormDistSink 0.01 0.01
    , testCase "100 Samples null is true." $ do
      lst <- sequence $ replicate 100 $ zeroSource $$ testNormDistSink 0.05 0.05
      let dts = foldl dtrFolder (initDTS $ head lst) $ tail lst
        in if (dtsValues dts) ! TestZero >= 85
           then return ()
           else assertFailure (show dts)      
    -- Note the type II error is currently higher than it should be
    -- when the actual difference is close to the 'minDiff'. This may
    -- be simply due to not accounting for the estimation of variance.
    , testCase "100 samples null is false." $ do
      lst <- sequence $ replicate 100 $ oneTenthSource $$ testNormDistSink 0.05 0.05
      let dts = foldl dtrFolder (initDTS $ head lst) $ tail lst
        in if (dtsValues dts) ! TestPositive >= 85
           then return ()
           else assertFailure (show dts)
    -}
    ]
  , testGroup "Tests for wilcoxon"
    [ {-testCase "Simple valid null hypothesis." $ {-do
         res <- tupleSource 0 0 $$ wilcoxonSink 0.01 0.05
         assertFailure $ show res-}
      assertResHasVal TestZero $ tupleSource 0 0 $$ wilcoxonSink 0.05 0.05
    , testCase "Simple invalid null hypothesis." $ {-do
         res <- tupleSource 0 0.1 $$ wilcoxonSink 0.01 0.05
         assertFailure $ show res-}
      assertResHasVal TestNegative $ tupleSource 0 0.1 $$ wilcoxonSink 0.05 0.05
    , -}testCase "Catching HLL error." $ do
      threadDelay 100000
      assertResHasVal TestNegative
        $ (CL.unfoldM (\_ -> do
                          pair <- generate genHLLActualApprox
                          return $ Just (pair, ())) ())
        =$ CL.map (\(actual, hll) -> 
                    let (Approximate conf lo _ hi) = HLL.size hll
                    in (conf, if lo <= actual && actual <= hi then 1 else 0))
        $$ wilcoxonSink 0.05 0.05
    ]
  ]

data SignedLog a = SignedLog {
    slSign :: S.Sign
  , slLn :: Log a
  }

instance (Show a, Floating a) => Show (SignedLog a) where
  show (SignedLog s a) = S.symbol s ++ if s == S.Zero then "" else show a

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
      (_, _, EQ) -> SignedLog S.Zero a
      (_, _, GT) -> SignedLog sA (a - b)
  (*) (SignedLog sA a) (SignedLog sB b) = SignedLog (S.mult sA sB) (a*b)

instance (Real a, Precise a, RealFloat a) => Real (SignedLog a) where
  toRational (SignedLog s a) = case s of
    S.Pos -> toRational a
    S.Zero -> 0
    S.Neg -> (-1) * (toRational a)

instance (Fractional a, Precise a, RealFloat a) => Fractional (SignedLog a) where
  fromRational r = case (signOf $ numerator r, signOf $ denominator r) of
    -- If sign is Zero then value is 0 reguardless of LN. Therefore if
    -- result actually wants to be some form of +/- inf or NaN the
    -- sign should not be Zero.
    (S.Zero, S.Zero) -> SignedLog S.Pos $ fromRational r
    (s, S.Zero) -> SignedLog s $ fromRational $ abs r
    (sA, sB) -> SignedLog (S.mult sA sB) $ fromRational $ abs r
  (SignedLog sA a) / (SignedLog sB b) = case sB of
    S.Pos -> SignedLog sA (a/b)
    S.Zero -> fromRational ((signToNum sA) % 0)
    S.Neg -> SignedLog (S.negate sA) (a/b)

signToNum :: (Num a) => S.Sign -> a
signToNum s = case s of
  S.Pos -> 1
  S.Zero -> 0
  S.Neg -> -1

instance (RealFrac a, Precise a, RealFloat a) => RealFrac (SignedLog a) where
  properFraction (SignedLog s a) = let (i,f) = properFraction a
                                     in (i * (signToNum s), SignedLog s f)

-- This wants to be replaced with a more accurate instance.
instance (InvErf a, Precise a, RealFloat a) => InvErf (Log a) where
  invnormcdf l = realToFrac $ invnormcdf (realToFrac l::a)

-- This wants to be replaced with a more accurate instance.
instance (RealFrac a, Precise a, RealFloat a) => RealFrac (Log a) where
  properFraction l = (\(b,a) -> (b, realToFrac a)) $ properFraction $ exp (ln l)


genHLLActualApprox :: (Num a) => Gen (a, HLL.HyperLogLog $(nat 5))
genHLLActualApprox = do
  lst <- arbitrary :: Gen [Integer]
  return (fromIntegral $ length $ nub lst, foldl (flip HLL.insert) mempty lst)

dtrFolder :: (Fractional a) => DistributionTestSummary a -> DistributionTestResult a -> DistributionTestSummary a
dtrFolder (DistributionTestSummary sVals sMeans sStdDevs sCounts sUppers sLowers)
  (DistributionTestResult nVal nMean nStdDev nCount nUpper nLower) =
  DistributionTestSummary (insertWith (+) nVal 1 sVals) (updateSSD nMean sMeans) (updateSSD nStdDev sStdDevs) (updateSSD (fromIntegral nCount) sCounts) (updateSSD nUpper sUppers) (updateSSD nLower sLowers)

assertResHasVal :: (Show n) => DistributionTestValue -> IO (DistributionTestResult n) -> Assertion
assertResHasVal a bIO = do
  b <- bIO
  if a == dtrValue b
    then return ()
    else assertFailure (  "Expected: " ++ (show a)
                       ++ "\nGot: " ++ (show b) )

tupleSource :: Double -> Double -> Source IO (Double, Double)
tupleSource a b = getZipSource $ ZipSource ((normalDoubleSource a) $= CL.map (\x y -> (x,y))) <*> ZipSource (normalDoubleSource b)

normalDoubleSource :: Double -> Source IO Double
normalDoubleSource mean = CL.unfoldM (\_ -> do
                                         r <- rIO
                                         return $ Just (r, ())) ()
  where rIO :: IO Double
        rIO = withSystemRandom (\gen -> normal mean 1 gen :: IO Double)

zeroSource :: Source IO Double
zeroSource = normalDoubleSource 0

oneTenthSource :: Source IO Double
oneTenthSource = normalDoubleSource 0.1
