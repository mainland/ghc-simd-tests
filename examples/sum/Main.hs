{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (evaluate)
import Data.Array.Parallel
import Data.Array.Parallel.PArray (nf, fromUArray)
import qualified Data.Vector.Unboxed as U
import System.IO (hFlush, stdout)
import System.Random (newStdGen, randomR)

import Util

#if defined(FLOAT)
#if defined(SCALAR)
import qualified Sum.Float.Scalar
#endif /* defined(SCALAR) */

#if defined(CSCALAR)
import qualified Sum.Float.CScalar
#endif /* defined(CSCALAR) */

#if defined(MANUAL)
import qualified Sum.Float.Manual
#endif /* defined(MANUAL) */

#if defined(CMANUAL)
import qualified Sum.Float.CManual
#endif /* defined(CMANUAL) */

#if defined(MULTIVECTOR)
import qualified Sum.Float.Multivector
#endif /* defined(MULTIVECTOR) */

#if defined(VECTOR)
import qualified Sum.Float.Vector
#endif /* defined(VECTOR) */
#endif /* defined(FLOAT) */

#if defined(DOUBLE)
#if defined(SCALAR)
import qualified Sum.Double.Scalar
#endif /* defined(SCALAR) */

#if defined(MANUAL)
import qualified Sum.Double.Manual
#endif /* defined(MANUAL) */

#if defined(MULTIVECTOR)
import qualified Sum.Double.Multivector
#endif /* defined(MULTIVECTOR) */

#if defined(VECTOR)
import qualified Sum.Double.Vector
#endif /* defined(VECTOR) */

#if defined(DPH)
import qualified Sum.Double.Dph
#endif /* defined(DPH) */

#if defined(DPHMULTI)
import qualified Sum.Double.DphMulti
#endif /* defined(DPHMULTI) */
#endif /* defined(DOUBLE) */

#if defined(INT64)
import Data.Int

#if defined(SCALAR)
import qualified Sum.Int64.Scalar
#endif /* defined(SCALAR) */

#if defined(MANUAL)
import qualified Sum.Int64.Manual
#endif /* defined(MANUAL) */

#if defined(MULTIVECTOR)
import qualified Sum.Int64.Multivector
#endif /* defined(MULTIVECTOR) */

#if defined(VECTOR)
import qualified Sum.Int64.Vector
#endif /* defined(VECTOR) */

#if defined(DPH)
import qualified Sum.Int64.Dph
#endif /* defined(DPH) */

#if defined(DPHMULTI)
import qualified Sum.Int64.DphMulti
#endif /* defined(DPHMULTI) */
#endif /* defined(INT64) */

nTRIALS :: Int
nTRIALS = 100

main :: IO ()
main = do
    -- generate random input vectors
#if defined(FLOAT)
    fu :: U.Vector Float      <-  randomU n range
#endif /* defined(FLOAT) */

#if defined(DOUBLE)
    du :: U.Vector Double      <-  randomU n range
    let dupa :: PArray Double  =   fromUArray du
#endif /* defined(DOUBLE) */

#if defined(INT64)
    i64u :: U.Vector Int64      <-  randomU n range
#endif /* defined(INT64) */

    putStr "Generating random vectors..."
    hFlush stdout
#if defined(FLOAT)
    evaluate fu
#endif /* defined(FLOAT) */
#if defined(DOUBLE)
    evaluate du
#endif /* defined(DOUBLE) */
#if defined(INT64)
    evaluate i64u
#endif /* defined(INT64) */
    putStrLn "done."
#if defined(DOUBLE)
    putStr "Converting to parallel arrays..."
    evaluate $ nf dupa
    putStrLn "done."
#endif /* defined(DOUBLE) */
    hFlush stdout

#if defined(FLOAT)
#if defined(SCALAR)
    runBenchmark nTRIALS "            sum Float scalar" Sum.Float.Scalar.sum       fu
#endif /* defined(SCALAR) */

#if defined(CSCALAR)
    runBenchmark nTRIALS "          sum Float C scalar" Sum.Float.CScalar.sum      fu
#endif /* defined(CSCALAR) */

#if defined(MANUAL)
    runBenchmark nTRIALS "       sum Float SIMD manual" Sum.Float.Manual.sum       fu
#endif /* defined(MANUAL) */

#if defined(CMANUAL)
    runBenchmark nTRIALS "     sum Float SIMD C manual" Sum.Float.CManual.sum      fu
#endif /* defined(CMANUAL) */

#if defined(MULTIVECTOR)
    runBenchmark nTRIALS "  sum Float SIMD multivector" Sum.Float.Multivector.sum  fu
#endif /* defined(MULTIVECTOR) */

#if defined(VECTOR)
    runBenchmark nTRIALS "       sum Float SIMD vector" Sum.Float.Vector.sum       fu
#endif /* defined(VECTOR) */
#endif /* defined(FLOAT) */

#if defined(DOUBLE)
#if defined(SCALAR)
    runBenchmark nTRIALS "           sum Double scalar" Sum.Double.Scalar.sum      du
#endif /* defined(SCALAR) */

#if defined(MANUAL)
    runBenchmark nTRIALS "      sum Double SIMD manual" Sum.Double.Manual.sum      du
#endif /* defined(MANUAL) */

#if defined(MULTIVECTOR)
    runBenchmark nTRIALS " sum Double SIMD multivector" Sum.Double.Multivector.sum du
#endif /* defined(MULTIVECTOR) */

#if defined(VECTOR)
    runBenchmark nTRIALS "      sum Double SIMD vector" Sum.Double.Vector.sum      du
#endif /* defined(VECTOR) */

#if defined(DPH)
    runBenchmark nTRIALS "       sum Double scalar DPH" Sum.Double.Dph.sum         du
#endif /* defined(DPH) */

#if defined(DPHMULTI)
    runBenchmark nTRIALS "         sum Double SIMD DPH" Sum.Double.DphMulti.sum    du
#endif /* defined(DPHMULTI) */
#endif /* defined(DOUBLE) */

#if defined(INT64)
#if defined(SCALAR)
    runBenchmark nTRIALS "           sum Int64 scalar" Sum.Int64.Scalar.sum      i64u
#endif /* defined(SCALAR) */

#if defined(MANUAL)
    runBenchmark nTRIALS "      sum Int64 SIMD manual" Sum.Int64.Manual.sum      i64u
#endif /* defined(MANUAL) */

#if defined(MULTIVECTOR)
    runBenchmark nTRIALS " sum Int64 SIMD multivector" Sum.Int64.Multivector.sum i64u
#endif /* defined(MULTIVECTOR) */

#if defined(VECTOR)
    runBenchmark nTRIALS "      sum Int64 SIMD vector" Sum.Int64.Vector.sum      i64u
#endif /* defined(VECTOR) */

#if defined(DPH)
    runBenchmark nTRIALS "       sum Int64 scalar DPH" Sum.Int64.Dph.sum         i64u
#endif /* defined(DPH) */

#if defined(DPHMULTI)
    runBenchmark nTRIALS "         sum Int64 SIMD DPH" Sum.Int64.DphMulti.sum    i64u
#endif /* defined(DPHMULTI) */
#endif /* defined(INT64) */
  where
    n :: Int
    n = 10000000     -- vector length

    range :: Num a => (a, a)
    range = (-100, 100)  -- range of vector elements
