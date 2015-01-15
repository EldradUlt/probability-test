{-# LANGUAGE TemplateHaskell, DataKinds, ScopedTypeVariables #-}
module Main where

import Test.ProbabilityCheck
import Test.Tasty.ProbabilityCheck
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (HUnitFailure(..), Assertion, testCase, assertFailure)
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
import qualified Data.Sign as S
import Data.Ratio (numerator, denominator)
import Control.Exception (try, SomeException)
import Data.List (isPrefixOf)

main :: IO ()
main =
  defaultMain $
  testGroup "probability-test's Tests"
  [ {-testGroup "Tests for testNormDistSink"
    [ testCase "Zero simple case" $ assertResHasVal TestZero $ zeroSource $$ testNormDistSink True 0.01 (MDAbsolute 0.01)
    , testCase "Positive simple case" $ assertResHasVal TestPositive $ oneTenthSource $$ testNormDistSink True 0.01 (MDAbsolute 0.01)
    , testCase "100 Samples null is true." $ do
      lst <- sequence $ replicate 100 $ zeroSource $$ testNormDistSink True 0.05 (MDAbsolute 0.05)
      let dts = foldl dtrFolder (initDTS $ head lst) $ tail lst
        in if (dtsValues dts) ! TestZero >= 85
           then return ()
           else assertFailure (show dts)      
    -- Note the type II error is currently higher than it should be
    -- when the actual difference is close to the 'minDiff'. This may
    -- be simply due to not accounting for the estimation of variance.
    , testCase "100 samples null is false." $ do
      lst <- sequence $ replicate 100 $ oneTenthSource $$ testNormDistSink True 0.05 (MDAbsolute 0.05)
      let dts = foldl dtrFolder (initDTS $ head lst) $ tail lst
        in if (dtsValues dts) ! TestPositive >= 85
           then return ()
           else assertFailure (show dts)
    ]
  , testGroup "Tests for wilcoxon"
    [ testCase "Simple valid null hypothesis." $
      assertResHasVal TestZero $ tupleSource 0 0 $$ wilcoxonSink 40 0.05 0.15
    , testCase "Simple invalid null hypothesis." $
      assertResHasVal TestNegative $ tupleSource 0 0.1 $$ wilcoxonSink 40 0.05 0.15
    , testCase "Catching HLL error." $ do
      assertResHasVal TestZero
        $ (CL.unfoldM (\_ -> do
                          (pair, _) <- generate genHLLActualApprox
                          return $ Just (pair, ())) ())
        =$ CL.map (\(actual, hll) ->
                    let (Approximate conf lo _ hi) = HLL.size hll
                    in (realToFrac conf, if lo <= actual && actual <= hi then 1 else 0) :: (SignedLog Double, SignedLog Double))
        $$ wilcoxonSink 10000 0.1 0.20
    ]
  , -}testGroup "Tests for testProbability"
    [ testProbabilistic "Simple testProbabilistic success."
      (ProbabilisticTest {
          ptS = rIO 0
          , ptA = 0.05
          , ptMD = MDAbsolute 0.05
          , ptNF = (\dtr -> Just $ "Actual tested value was less than expected value.\n" ++ show dtr)
          , ptPF = (\dtr -> Just $ "Actual tested value was greater than expected value.\n" ++ show dtr)
          }
       )
    , testCase "Simple testProbabilistic failure." $ do
        e <- try (defaultMain $ testProbabilistic "" --Should make this quiet so that it doesn't print the confusing failure.
                  (ProbabilisticTest {
                      ptS = rIO 0.1
                      , ptA = 0.05
                      , ptMD = MDAbsolute 0.05
                      , ptNF = (\dtr -> Just $ "Actual tested value was less than expected value.\n" ++ show dtr)
                      , ptPF = (\dtr -> Just $ "Actual tested value was greater than expected value.\n" ++ show dtr)
                      }
                  )
                 ) :: IO (Either SomeException ())
        case e of
          Right _ -> assertFailure "Test should have failed but succeeded."
          Left _ -> return () -- Really would prefer to check that it got the RIGHT failure, but not sure how to do that with Tasty.
          --Left (HUnitFailure s) -> if isPrefixOf "Actual tested value was greater than expected value." s then return ()
                                   --else assertFailure $ "Failed for unexpected reason:\n" ++ s
    , testProbabilistic "normDistToProbTestable test." $ normDistToProbTestable 0.05 0.05 (rIO 0)
    ]
  ]

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

genHLLActualApprox :: (Num a) => Gen ((a, HLL.HyperLogLog $(nat 5)), [Integer])
genHLLActualApprox = do
  lst <- arbitrary :: Gen [Integer]
  return ((fromIntegral $ length $ nub lst, foldl (flip HLL.insert) mempty lst), lst)

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
                                         r <- rIO mean
                                         return $ Just (r, ())) ()

rIO :: Double -> IO Double
rIO mean = withSystemRandom (\gen -> normal mean 1 gen :: IO Double)

zeroSource :: Source IO Double
zeroSource = normalDoubleSource 0

oneTenthSource :: Source IO Double
oneTenthSource = normalDoubleSource 0.1
