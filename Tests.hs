{-# LANGUAGE TemplateHaskell, DataKinds, ScopedTypeVariables #-}
module Main where

import Test.ProbabilityCheck
import Test.Tasty.ProbabilityCheck
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, assertFailure)
import Data.Conduit (($$), Source, ZipSource(..), ($=))
import qualified Data.Conduit.List as CL
import System.Random.MWC (withSystemRandom)
import System.Random.MWC.Distributions (normal)
import Data.Map.Strict (insertWith, (!))
import Control.Applicative ((<*>))
import qualified Data.HyperLogLog as HLL
import Data.Reflection (nat)
import Data.Approximate (Approximate(..))
import Data.List (nub)
import Test.QuickCheck (Gen, arbitrary, choose, elements, frequency)
import Data.Monoid (mempty)
import Control.Exception (try, SomeException)
import Data.Ratio (numerator, denominator)

main :: IO ()
main =
  defaultMain $
  testGroup "probability-test's Tests"
  [ testGroup "Tests for testNormDistSink"
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
    {- This test is incorrectly written and needs to be replaced.
    , testCase "Catching HLL error." $ do
      assertResHasVal TestZero
        $ (CL.unfoldM (\_ -> do
                          (pair, _) <- generate genHLLActualApprox
                          return $ Just (pair, ())) ())
        =$ CL.map (\(actual, hll) ->
                    let (Approximate conf lo _ hi) = HLL.size hll
                    in (realToFrac conf, if lo <= actual && actual <= hi then 1 else 0) :: (SignedLog Double, SignedLog Double))
        $$ wilcoxonSink 10000 0.1 0.20
    -}
    ]
  , testGroup "Tests for testProbability"
    [ testProbabilistic "Simple testProbabilistic success."
      (ProbabilisticTest {
          ptS =  CL.unfoldM (\_ -> (rIO 0) >>= (\a -> return $ Just (a,()))) ()
          , ptA = 0.05
          , ptMD = MDAbsolute 0.05
          , ptNF = (\dtr -> Just $ "Actual tested value was less than expected value.\n" ++ show dtr)
          , ptPF = (\dtr -> Just $ "Actual tested value was greater than expected value.\n" ++ show dtr)
          }
       )
    , testCase "Simple testProbabilistic failure." $ do
        e <- try (defaultMain $ testProbabilistic "" --Should make this quiet so that it doesn't print the confusing failure.
                  (ProbabilisticTest {
                      ptS = CL.unfoldM (\_ -> (rIO 0.1) >>= (\a -> return $ Just (a,()))) ()
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
    , testProbabilistic "pairsToProbTestable IO test."
      $ pairsToProbTestable 0.05 (40::Integer) (rIO 0 >>= (\r1-> rIO 0 >>= (\r2-> return (r1, r2))))
    , testProbabilistic "pairsToProbTestable test." $ pairsToProbTestable 0.05 (40::Integer) (pIO 0 0)
    , testProbabilistic "HLL test with testProbabilistic." $ pairsToProbTestable 0.05 (40::Integer) genHLLConfCorrect
    ]
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

pIO :: Double -> Double -> Gen (Double, Double)
pIO meanA meanB = do
  a <- elements [meanA-1, meanA+1] -- Binary distribution containing points of meanA +/- 1.
  b <- choose (meanB-1,meanB+1) -- Uniform distribution over meanB +/- 1.
  return (a, b)

zeroSource :: Source IO Double
zeroSource = normalDoubleSource 0

oneTenthSource :: Source IO Double
oneTenthSource = normalDoubleSource 0.1
