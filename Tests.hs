{-# LANGUAGE TemplateHaskell, DataKinds, ScopedTypeVariables, DeriveDataTypeable #-}
module Main where

--import Test.ProbabilityCheck
import Test.ProbabilityCheck.EBS
import Test.Tasty.ProbabilityCheck
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, assertString, testCase)
import Data.Conduit (Source, ZipSource(..), ($=), Conduit, ($$))
import qualified Data.Conduit.List as CL
import System.Random.MWC (withSystemRandom)
import System.Random.MWC.Distributions (normal)
import Control.Applicative ((<*>))
import qualified Data.HyperLogLog as HLL
import Data.Reflection (nat)
import Data.Approximate (Approximate(..))
import Data.List (nub)
import Test.QuickCheck (Gen, Arbitrary(..), choose, elements, frequency, sample')
import Data.Monoid (mempty)
import Data.Ratio (numerator, denominator)
import Data.Int (Int64)
import Data.Typeable (Typeable)

main :: IO ()
main =
  defaultMain $
  testGroup "probability-test's Tests"
  [
    testCase "Simple empiricalBernstienStopping TestZero case" $
    assertResHasVal TestZero $ zeroSourceRangeLimit $$ empiricalBernstienStopping 2 0.05 0.01
  , testCase "Simple empiricalBernstienStopping TestPositive case" $
    assertResHasVal TestPositive $ oneTenthSourceRangeLimit $$ empiricalBernstienStopping 2 0.05 0.01
  , testCase "Simple empiricalBernstienStopping TestNegative case" $
    assertResHasVal TestNegative $ negOneTenthSourceRangeLimit $$ empiricalBernstienStopping 2 0.05 0.01
  , testApproximate "HLL test." (HLL.size . foldl (flip HLL.insert) (mempty :: HLL.HyperLogLog $(nat 5))) (fromIntegral . length . nub :: [Int] -> Int64)
  , testApproximate "Approx simple pass." simpleCorrectApprox simpleActual
  , testApproximate "Approx simple fail." simpleIncorrectApprox simpleActual
  --, testCase "Foobar" $ sample' (arbitrary :: Gen ZeroToNintyNine) >>= assertString . show
  ]

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

tupleSource :: Double -> Double -> Source IO (Double, Double)
tupleSource a b = getZipSource $ ZipSource ((normalDoubleSource a) $= CL.map (\x y -> (x,y))) <*> ZipSource (normalDoubleSource b)

normalDoubleSource :: Double -> Source IO Double
normalDoubleSource mean = CL.unfoldM (\_ -> do
                                         r <- rIO mean
                                         return $ Just (r, ())) ()

limitRange :: Double -> Double -> Conduit Double IO Double
limitRange lo hi = CL.map (\a -> max lo $ min hi a)

rIO :: Double -> IO Double
rIO mean = withSystemRandom (\gen -> normal mean 0.1 gen :: IO Double)

pIO :: Double -> Double -> Gen (Double, Double)
pIO meanA meanB = do
  a <- elements [meanA-1, meanA+1] -- Binary distribution containing points of meanA +/- 1.
  b <- choose (meanB-1,meanB+1) -- Uniform distribution over meanB +/- 1.
  return (a, b)

zeroSource :: Source IO Double
zeroSource = normalDoubleSource 0

zeroSourceRangeLimit :: Source IO Double
zeroSourceRangeLimit = zeroSource $= limitRange (-1) 1

oneTenthSourceRangeLimit :: Source IO Double
oneTenthSourceRangeLimit = (normalDoubleSource 0.01) $= limitRange (-0.9) 1.1

negOneTenthSourceRangeLimit :: Source IO Double
negOneTenthSourceRangeLimit = (normalDoubleSource (-0.01)) $= limitRange (-1.1) 0.9
