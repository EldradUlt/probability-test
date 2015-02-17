{-# LANGUAGE TemplateHaskell, DataKinds, ScopedTypeVariables #-}
module Main where

import Test.ProbabilityCheck
import Test.Tasty.ProbabilityCheck
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure)
import Data.Conduit (Source, ZipSource(..), ($=))
import qualified Data.Conduit.List as CL
import System.Random.MWC (withSystemRandom)
import System.Random.MWC.Distributions (normal)
import Control.Applicative ((<*>))
import qualified Data.HyperLogLog as HLL
import Data.Reflection (nat)
import Data.Approximate (Approximate(..))
import Data.List (nub)
import Test.QuickCheck (Gen, arbitrary, choose, elements, frequency)
import Data.Monoid (mempty)
import Data.Ratio (numerator, denominator)

main :: IO ()
main =
  defaultMain $
  testGroup "probability-test's Tests"
  [
  ]

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

rIO :: Double -> IO Double
rIO mean = withSystemRandom (\gen -> normal mean 1 gen :: IO Double)

pIO :: Double -> Double -> Gen (Double, Double)
pIO meanA meanB = do
  a <- elements [meanA-1, meanA+1] -- Binary distribution containing points of meanA +/- 1.
  b <- choose (meanB-1,meanB+1) -- Uniform distribution over meanB +/- 1.
  return (a, b)

zeroSource :: Source IO Double
zeroSource = normalDoubleSource 0

oneTenthSource :: Source IO Double
oneTenthSource = normalDoubleSource 0.1
