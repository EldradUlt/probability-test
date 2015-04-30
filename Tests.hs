{-# LANGUAGE TemplateHaskell, DataKinds, ScopedTypeVariables, DeriveDataTypeable #-}
module Main where

--import Test.ProbabilityCheck
import Test.ProbabilityCheck.EBS
import Test.Tasty.ProbabilityCheck
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import Data.Conduit (Source, ZipSource(..), ($=), Conduit, ($$))
import qualified Data.Conduit.List as CL
import System.Random.MWC (createSystemRandom, GenIO)
import System.Random.MWC.Distributions (normal, uniformPermutation)
import Control.Applicative ((<*>))
import qualified Data.HyperLogLog as HLL
import Data.Reflection (nat)
import Data.Approximate (Approximate(..))
import Data.List (nub)
import Test.QuickCheck (Gen, Arbitrary(..), choose, elements, frequency)
import Data.Monoid (mempty)
import Data.Ratio (numerator, denominator)
import Data.Int (Int64)
import Data.Typeable (Typeable)
import Numeric.Log.Signed (SignedLog)
import qualified Data.Vector as V

main :: IO ()
main = do
  mwcGen <- createSystemRandom
  defaultMain $
    testGroup "probability-test's Tests"
    [
      testCase "Simple empiricalBernstienStopping TestZero case" $
      assertResHasVal TestZero $ zeroSourceRangeLimit mwcGen $$ empiricalBernstienStopping 2 0.05 0.01
    , testCase "Simple empiricalBernstienStopping TestPositive case" $
      assertResHasVal TestPositive $ oneHundrethSourceRangeLimit mwcGen $$ empiricalBernstienStopping 2 0.05 0.01
    , testCase "Simple empiricalBernstienStopping TestNegative case" $
      assertResHasVal TestNegative $ negOneHundrethSourceRangeLimit mwcGen $$ empiricalBernstienStopping 2 0.05 0.01
    , testApproximate "HLL test." (HLL.size . foldl (flip HLL.insert) (mempty :: HLL.HyperLogLog $(nat 5))) (fromIntegral . length . nub :: [Int] -> Int64)
    , testApproximate "Approx simple pass." simpleCorrectApprox simpleActual
    , testApproximate "Approx simple fail. *THIS SHOULD FAIL*" simpleIncorrectApprox simpleActual
    {-, testCase "Simulated 'Approx simple fail' with Doubles and MWC random." $
      assertResHasVal TestNegative $ simulatedApproxSource mwcGen (0.91::Double) $$ empiricalBernstienStopping 2 0.05 0.01
    , testCase "Simulated 'Approx simple fail' with SignedLog Doubles and MWC random." $
      assertResHasVal TestNegative $ simulatedApproxSource mwcGen (0.91::SignedLog Double) $$ empiricalBernstienStopping 2 0.05 0.01
    -}
    ]

simulatedApproxSource :: (Num a) => GenIO -> a -> Source IO a
simulatedApproxSource mwcGen conf = sourceZeroToNine mwcGen $= CL.map (\r -> (if r == 0 then 0 else 1) - conf)

sourceZeroToNine :: GenIO -> Source IO Int
sourceZeroToNine mwcGen = CL.unfoldM (\_ -> do
                                         lst <- uniformPermutation 10 mwcGen
                                         return $ Just (V.head lst, ())) ()

simpleCorrectApprox :: a -> Approximate Int
simpleCorrectApprox _ = Approximate 0.9 10 55 99

simpleIncorrectApprox :: a -> Approximate Int
simpleIncorrectApprox _ = Approximate 0.91 10 55 99

data ZeroToNintyNine = ZeroToNintyNine {simpleActual :: Int} deriving (Typeable)

instance Show ZeroToNintyNine where
  show = show . simpleActual

instance Arbitrary ZeroToNintyNine where
  arbitrary = choose (0,99) >>= return . ZeroToNintyNine

genHLLConfCorrect :: Gen (SignedLog Double, SignedLog Double)
genHLLConfCorrect = do
  ((actual, hll), _) <- genHLLActualApprox
  let (Approximate conf lo _ hi) = HLL.size hll
      (n, d) = (numerator $ toRational conf, denominator $ toRational conf)
      (a, b) = (fromInteger (d-n), fromInteger n)
  c <- frequency [(a, return 0), (b, return 1)]
  return (c, if lo <= actual && actual <= hi then 1 else 0)

genHLLActualApprox :: (Num a) => Gen ((a, HLL.HyperLogLog $(nat 5)), [Integer])
genHLLActualApprox = do
  lst <- arbitrary :: Gen [Integer]
  return ((fromIntegral $ length $ nub lst, foldl (flip HLL.insert) mempty lst), lst)

assertResHasVal :: (Show n) => DistributionTestValue -> IO (DistributionTestResult n) -> Assertion
assertResHasVal a bIO = do
  b <- bIO
  if a == dtrValue b
    then return ()
    else assertFailure (  "Expected: " ++ (show a)
                       ++ "\nGot: " ++ (show b) )

tupleSource :: GenIO -> Double -> Double -> Source IO (Double, Double)
tupleSource mwcGen a b = getZipSource $ ZipSource ((normalDoubleSource mwcGen a) $= CL.map (\x y -> (x,y))) <*> ZipSource (normalDoubleSource mwcGen b)

normalDoubleSource :: GenIO -> Double -> Source IO Double
normalDoubleSource mwcGen mean = CL.unfoldM (\_ -> do
                                         r <- rIO mwcGen mean
                                         return $ Just (r, ())) ()

limitRange :: Double -> Double -> Conduit Double IO Double
limitRange lo hi = CL.map (\a -> max lo $ min hi a)

rIO :: GenIO -> Double -> IO Double
rIO mwcGen mean = normal mean 0.1 mwcGen

pIO :: Double -> Double -> Gen (Double, Double)
pIO meanA meanB = do
  a <- elements [meanA-1, meanA+1] -- Binary distribution containing points of meanA +/- 1.
  b <- choose (meanB-1,meanB+1) -- Uniform distribution over meanB +/- 1.
  return (a, b)

zeroSource :: GenIO -> Source IO Double
zeroSource mwcGen = normalDoubleSource mwcGen 0

zeroSourceRangeLimit :: GenIO -> Source IO Double
zeroSourceRangeLimit mwcGen = zeroSource mwcGen $= limitRange (-1) 1

oneHundrethSourceRangeLimit :: GenIO -> Source IO Double
oneHundrethSourceRangeLimit mwcGen = (normalDoubleSource mwcGen 0.01) $= limitRange (-0.9) 1.1

negOneHundrethSourceRangeLimit :: GenIO -> Source IO Double
negOneHundrethSourceRangeLimit mwcGen  = (normalDoubleSource mwcGen (-0.01)) $= limitRange (-1.1) 0.9
