{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (evaluate)
import qualified Data.Vector.Unboxed as U
import System.IO (hFlush, stdout)
import System.Random (newStdGen, randomR)

import Util

#if defined(FLOAT)
#if defined(SCALAR)
import qualified Saxpy.Float.Scalar
#endif /* defined(SCALAR) */

#if defined(MULTIVECTOR)
import qualified Saxpy.Float.Multivector
#endif /* defined(MULTIVECTOR) */

#if defined(VECTOR)
import qualified Saxpy.Float.Vector
#endif /* defined(VECTOR) */
#endif /* defined(FLOAT) */

nTRIALS :: Int
nTRIALS = 100

main :: IO ()
main = do
    -- generate random input vectors
    g                         <-  newStdGen
#if defined(FLOAT)
    let fa :: Float           =   fst (randomR range g)
    fu :: U.Vector Float      <-  randomU n range
    fv :: U.Vector Float      <-  randomU n range
#endif /* defined(FLOAT) */

    putStr "Generating random vectors..."
    hFlush stdout
#if defined(FLOAT)
    evaluate fa
    evaluate fu
    evaluate fv
#endif /* defined(FLOAT) */
    putStrLn "done."
    hFlush stdout

#if defined(FLOAT)
#if defined(SCALAR)
    runBenchmark nTRIALS "            saxpy Float scalar" (\(a, x, y) -> Saxpy.Float.Scalar.saxpy a x y)      (fa, fu, fv)
#endif /* defined(SCALAR) */

#if defined(MULTIVECTOR)
    runBenchmark nTRIALS "   saxpy Float SIMD mutivector" (\(a, x, y) -> Saxpy.Float.Multivector.saxpy a x y) (fa, fu, fv)
#endif /* defined(MULTIVECTOR) */

#if defined(VECTOR)
    runBenchmark nTRIALS "       saxpy Float SIMD vector" (\(a, x, y) -> Saxpy.Float.Vector.saxpy a x y)      (fa, fu, fv)
#endif /* defined(VECTOR) */
#endif /* defined(FLOAT) */
  where
    n     = 10000000     -- vector length
    range = (-100, 100)  -- range of vector elements
