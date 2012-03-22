{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (evaluate)
import Data.Array.Parallel
import Data.Array.Parallel.Array
import "dph-lifted-vseg" Data.Array.Parallel.PArray (nf, fromUArray)
import "dph-prim-par" Data.Array.Parallel.Unlifted
import qualified Data.Vector.Unboxed as U
import qualified Data.Array.Parallel.Unlifted as U
import System.IO (hFlush, stdout)
import System.Random (newStdGen, randomR)

import Util

#if defined(FLOAT)
#if defined(SCALAR)
import qualified Dotp.Float.Scalar
#endif /* defined(SCALAR) */

#if defined(CSCALAR)
import qualified Dotp.Float.CScalar
#endif /* defined(CSCALAR) */

#if defined(MANUAL)
import qualified Dotp.Float.Manual
#endif /* defined(MANUAL) */

#if defined(CMANUAL)
import qualified Dotp.Float.CManual
#endif /* defined(CMANUAL) */

#if defined(MULTIVECTOR)
import qualified Dotp.Float.Multivector
#endif /* defined(MULTIVECTOR) */

#if defined(VECTOR)
import qualified Dotp.Float.Vector
#endif /* defined(VECTOR) */

#if defined(VECTORALT1)
import qualified Dotp.Float.VectorAlt1
#endif /* defined(VECTORALT1) */

#if defined(VECTORALT2)
import qualified Dotp.Float.VectorAlt2
#endif /* defined(VECTORALT2) */

#if defined(VECTORALT3)
import qualified Dotp.Float.VectorAlt3
#endif /* defined(VECTORALT3) */
#endif /* defined(FLOAT) */

#if defined(DOUBLE)
#if defined(SCALAR)
import qualified Dotp.Double.Scalar
#endif /* defined(SCALAR) */

#if defined(MANUAL)
import qualified Dotp.Double.Manual
#endif /* defined(MANUAL) */

#if defined(MULTIVECTOR)
import qualified Dotp.Double.Multivector
#endif /* defined(MULTIVECTOR) */

#if defined(VECTOR)
import qualified Dotp.Double.Vector
#endif /* defined(VECTOR) */

#if defined(DPH)
import qualified Dotp.Double.Dph
#endif /* defined(DPH) */

#if defined(DPHMULTI)
import qualified Dotp.Double.DphMulti
#endif /* defined(DPHMULTI) */
#endif /* defined(DOUBLE) */

nTRIALS :: Int
nTRIALS = 100

main :: IO ()
main = do
    -- generate random input vectors
#if defined(FLOAT)
    fu :: U.Vector Float      <-  randomU n range
    fv :: U.Vector Float      <-  randomU n range
#endif /* defined(FLOAT) */

#if defined(DOUBLE)
    du :: U.Vector Double      <-  randomU n range
    dv :: U.Vector Double      <-  randomU n range
    let dupa :: PArray Double  =   fromUArray du
    let dvpa :: PArray Double  =   fromUArray dv
#endif /* defined(DOUBLE) */

    putStr "Generating random vectors..."
    hFlush stdout
#if defined(FLOAT)
    evaluate fu
    evaluate fv
#endif /* defined(FLOAT) */
#if defined(DOUBLE)
    evaluate du
    evaluate dv
#endif /* defined(DOUBLE) */
    putStrLn "done."
#if defined(DOUBLE)
    putStr "Converting to parallel arrays..."
    evaluate $ nf dupa
    evaluate $ nf dvpa
    putStrLn "done."
#endif /* defined(DOUBLE) */
    hFlush stdout

#if defined(FLOAT)
#if defined(SCALAR)
    runBenchmark nTRIALS "            dotp Float scalar" (uncurry Dotp.Float.Scalar.dotp)       (fu, fv)
#endif /* defined(SCALAR) */

#if defined(CSCALAR)
    runBenchmark nTRIALS "          dotp Float C scalar" (uncurry Dotp.Float.CScalar.dotp)      (fu, fv)
#endif /* defined(CSCALAR) */

#if defined(MANUAL)
    runBenchmark nTRIALS "       dotp Float SIMD manual" (uncurry Dotp.Float.Manual.dotp)       (fu, fv)
#endif /* defined(MANUAL) */

#if defined(CMANUAL)
    runBenchmark nTRIALS "     dotp Float SIMD C manual" (uncurry Dotp.Float.CManual.dotp)      (fu, fv)
#endif /* defined(CMANUAL) */

#if defined(MULTIVECTOR)
    runBenchmark nTRIALS "  dotp Float SIMD multivector" (uncurry Dotp.Float.Multivector.dotp)  (fu, fv)
#endif /* defined(MULTIVECTOR) */

#if defined(VECTOR)
    runBenchmark nTRIALS "       dotp Float SIMD vector" (uncurry Dotp.Float.Vector.dotp)       (fu, fv)
#endif /* defined(VECTOR) */

#if defined(VECTORALT1)
    runBenchmark nTRIALS " dotp Float SIMD vector alt 1" (uncurry Dotp.Float.VectorAlt1.dotp)   (fu, fv)
#endif /* defined(VECTORALT1) */

#if defined(VECTORALT2)
    runBenchmark nTRIALS " dotp Float SIMD vector alt 2" (uncurry Dotp.Float.VectorAlt2.dotp)   (fu, fv)
#endif /* defined(VECTORALT2) */

#if defined(VECTORALT3)
    runBenchmark nTRIALS " dotp Float SIMD vector alt 3" (uncurry Dotp.Float.VectorAlt3.dotp)   (fu, fv)
#endif /* defined(VECTORALT3) */
#endif /* defined(FLOAT) */

#if defined(DOUBLE)
#if defined(SCALAR)
    runBenchmark nTRIALS "           dotp Double scalar" (uncurry Dotp.Double.Scalar.dotp)      (du, dv)
#endif /* defined(SCALAR) */

#if defined(MANUAL)
    runBenchmark nTRIALS "      dotp Double SIMD manual" (uncurry Dotp.Double.Manual.dotp)      (du, dv)
#endif /* defined(MANUAL) */

#if defined(MULTIVECTOR)
    runBenchmark nTRIALS " dotp Double SIMD multivector" (uncurry Dotp.Double.Multivector.dotp) (du, dv)
#endif /* defined(MULTIVECTOR) */

#if defined(VECTOR)
    runBenchmark nTRIALS "      dotp Double SIMD vector" (uncurry Dotp.Double.Vector.dotp)      (du, dv)
#endif /* defined(VECTOR) */

#if defined(DPH)
    runBenchmark nTRIALS "       dotp Double scalar DPH" (uncurry Dotp.Double.Dph.dotp)         (dupa, dvpa)
#endif /* defined(DPH) */

#if defined(DPHMULTI)
    runBenchmark nTRIALS "         dotp Double SIMD DPH" (uncurry Dotp.Double.DphMulti.dotp)    (dupa, dvpa)
#endif /* defined(DPHMULTI) */
#endif /* defined(DOUBLE) */
  where
    n :: Int
    n = 10000000     -- vector length

    range :: Num a => (a, a)
    range = (-100, 100)  -- range of vector elements
