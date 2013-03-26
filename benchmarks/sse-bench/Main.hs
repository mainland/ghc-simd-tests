{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (evaluate)
import Control.Monad (when)
import Foreign (sizeOf)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Printf

import qualified Data.Vector.Unboxed as V

import Util.Benchmark
import Util.Random

import qualified Sum.Scalar
import qualified Sum.SSE

import qualified Kahan.Scalar
import qualified Kahan.SSE

import qualified Quickhull.Solver.Scalar
import qualified Quickhull.Solver.SSE

nTRIALS :: Int
nTRIALS = 100

main :: IO ()
main = do
    args <- getArgs
    mapM_ (runN args) (map (round . (2**)) [16 :: Float])
  where
    runN :: [String] -> Int -> IO ()
    runN args n = do
        -- Generate random input vectors
        du :: V.Vector Double <-  randomU n range
        dv :: V.Vector Double <-  randomU n range

        evaluate du
        evaluate dv

        -- Run the benchmarks
        runOne "sum" "scalar" n Sum.Scalar.sum du
        runOne "sum" "sse"    n Sum.SSE.sum    du

        runOne "kahan" "scalar" n Kahan.Scalar.sum du
        runOne "kahan" "sse"    n Kahan.SSE.sum    du

        runOne "quickhull" "scalar" n Quickhull.Solver.Scalar.quickhull (du, dv)
        runOne "quickhull" "sse"    n Quickhull.Solver.SSE.quickhull    (du, dv)

    runOne  ::  String
            ->  String
            ->  Int
            ->  (a -> b)
            ->  a
            ->  IO ()
    runOne func variant n f x = do
        (mean, max, min, sigma) <- timeBenchmark CPUClock nTRIALS (Just 1.0) f x
        printf "%s,%s,%d,%02f,%02f,%02f,%02f\n" func variant n mean min max sigma

    range :: Num a => (a, a)
    range = (0, 1)  -- range of vector elements
