module Main where

import Test.ProbabilityCheck
import Test.Tasty
import Test.Tasty.HUnit
import Data.Conduit (($$), Source)
import qualified Data.Conduit.List as CL
import System.Random.MWC (withSystemRandom)
import System.Random.MWC.Distributions (normal)
import Data.Map.Strict (insertWith, (!))

main :: IO ()
main =
  defaultMain $
  testGroup "probability-test's Tests"
  [ testGroup "Tests for testNormDistSink"
    [ testCase "Zero simple case" $ assertResHasVal TestZero $ zeroSource $$ testNormDistSink 0.05 0.05
    , testCase "Positive simple case" $ assertResHasVal TestPositive $ oneTenthSource $$ testNormDistSink 0.05 0.05
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
    ]
  ]

dtrFolder :: (Fractional a) => DistributionTestSummary a -> DistributionTestResult a -> DistributionTestSummary a
dtrFolder (DistributionTestSummary sVals sMeans sStdDevs sCounts sUppers sLowers)
  (DistributionTestResult nVal nMean nStdDev nCount nUpper nLower) =
  DistributionTestSummary (insertWith (+) nVal 1 sVals) (updateSSD nMean sMeans) (updateSSD nStdDev sStdDevs) (updateSSD (fromIntegral nCount) sCounts) (updateSSD nUpper sUppers) (updateSSD nLower sLowers)

assertResHasVal :: DistributionTestValue -> IO (DistributionTestResult Double) -> Assertion
assertResHasVal a bIO = do
  b <- bIO
  if a == dtrValue b
    then return ()
    else assertFailure (  "Expected: " ++ (show a)
                       ++ "\nGot: " ++ (show b) )

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
