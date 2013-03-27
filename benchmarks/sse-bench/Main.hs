{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Monad (when)
import Foreign (sizeOf)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Printf

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Util.Benchmark
import Util.Random

import qualified Sum.Scalar
import qualified Sum.SSE

import qualified Kahan.Scalar
import qualified Kahan.SSE

import qualified Dotp.Scalar
import qualified Dotp.SSE

import qualified Saxpy.Scalar
import qualified Saxpy.SSE

import qualified Rbf.Scalar
import qualified Rbf.SSE

import qualified Quickhull.Solver.Scalar
import qualified Quickhull.Solver.SSE

nTRIALS :: Int
nTRIALS = 100

-- Fraction of matrix entries that should be non-zero
sPARSE_FRAC :: Double
sPARSE_FRAC = 0.10

type SparseMatrix a = V.Vector (U.Vector (Int, a))

main :: IO ()
main = do
    args <- getArgs
    mapM_ (runN args) (map (round . (2**)) [16 :: Float])
  where
    runN :: [String] -> Int -> IO ()
    runN args n = do
        -- Generate random input vectors
        du1 :: U.Vector Double <-  randomU n range1
        dv1 :: U.Vector Double <-  randomU n range1

        du2 :: U.Vector Double <-  randomU n range2
        dv2 :: U.Vector Double <-  randomU n range2

        evaluate du1
        evaluate dv1

        evaluate du2
        evaluate dv2

        let lf = sqrt (fromIntegral n / sPARSE_FRAC)
            l  = truncate lf
            k  = truncate (0.10 * lf)

        m :: SparseMatrix Double <- randomSparseMatrix k l range1
        v :: U.Vector Double     <- randomU l range1

        evaluate $ rnf m

        -- Run the benchmarks
        runOne "sum" "scalar" n Sum.Scalar.sum du1
        runOne "sum" "sse"    n Sum.SSE.sum    du1

        runOne "kahan" "scalar" n Kahan.Scalar.sum du1
        runOne "kahan" "sse"    n Kahan.SSE.sum    du1

        runOne "dotp" "scalar" n (\(u,v) -> Dotp.Scalar.dotp u v) (du1, dv1)
        runOne "dotp" "sse"    n (\(u,v) -> Dotp.SSE.dotp u v)    (du1, dv1)

        runOne "saxpy" "scalar" n (\(u,v) -> Saxpy.Scalar.saxpy 0.1 u v) (du1, dv1)
        runOne "saxpy" "sse"    n (\(u,v) -> Saxpy.SSE.saxpy 0.1 u v)    (du1, dv1)

        runOne "rbf" "scalar" n (\(u,v) -> Rbf.Scalar.rbf nu u v) (du2, dv2)
        runOne "rbf" "sse"    n (\(u,v) -> Rbf.SSE.rbf nu u v)    (du2, dv2)

        runOne "quickhull" "scalar" n Quickhull.Solver.Scalar.quickhull (du1, dv1)
        runOne "quickhull" "sse"    n Quickhull.Solver.SSE.quickhull    (du1, dv1)

    runOne  ::  String
            ->  String
            ->  Int
            ->  (a -> b)
            ->  a
            ->  IO ()
    runOne func variant n f x = do
        (mean, max, min, sigma) <- timeBenchmark CPUClock nTRIALS (Just 1.0) f x
        printf "%s,%s,%d,%02f,%02f,%02f,%02f\n" func variant n mean min max sigma

    range1 :: Num a => (a, a)
    range1 = (-100, 100)  -- range of vector elements

    range2 :: Num a => (a, a)
    range2 = (0, 1)  -- range of vector elements

    nu :: Double
    nu = 0.001
