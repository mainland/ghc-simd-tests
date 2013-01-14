{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (evaluate)
import Data.Array.Parallel
import Data.Array.Parallel.PArray (nf, fromUArray)
import Data.Int
import qualified Data.Vector.Unboxed as U
import System.IO (hFlush, stdout)
import System.Random (newStdGen, randomR)
import Text.Printf

import Util.Random
import Util.Statistics

import qualified Saxpy.Float.Scalar
import qualified Saxpy.Float.Vector

import qualified Dotp.Float.Scalar
import qualified Dotp.Float.CScalar
import qualified Dotp.Float.Manual
import qualified Dotp.Float.CManual
import qualified Dotp.Float.Vector

import qualified Dotp.Double.Scalar
import qualified Dotp.Double.Manual
import qualified Dotp.Double.CManual
import qualified Dotp.Double.CBlas
import qualified Dotp.Double.Vector

import qualified Dotp.Double.Dph
import qualified Dotp.Double.DphPA
import qualified Dotp.Double.DphMulti

import qualified Sum.Int64.Scalar
import qualified Sum.Int64.Vector

import qualified Rbf.Double.CManual
import qualified Rbf.Double.Vector
import qualified Rbf.Double.Scalar

nTRIALS :: Int
nTRIALS = 100

main :: IO ()
main = do
    -- generate random input vectors
    g                         <-  newStdGen

    let fa :: Float           =   fst (randomR range g)
    fu :: U.Vector Float      <-  randomU n range
    fv :: U.Vector Float      <-  randomU n range

    du :: U.Vector Double     <-  randomU n range
    dv :: U.Vector Double     <-  randomU n range
    du2 :: U.Vector Double    <-  randomU n range2
    dv2 :: U.Vector Double    <-  randomU n range2
    let dupa :: PArray Double =   fromUArray du
    let dvpa :: PArray Double =   fromUArray dv

    i64u :: U.Vector Int64    <-  randomU n range

    putStr "Generating random vectors..."
    hFlush stdout
    evaluate fu
    evaluate fv
    evaluate du
    evaluate dv
    evaluate du2
    evaluate dv2
    putStrLn "done."
    putStr "Converting to parallel arrays..."
    evaluate $ nf dupa
    evaluate $ nf dvpa
    putStrLn "done."
    hFlush stdout

    let saxpy_golden = Saxpy.Float.Scalar.saxpy fa fu fv
    let saxpy_vector = Saxpy.Float.Vector.saxpy fa fu fv
    printf "scalar saxpy (Float) L1 norm: %f\n" (l1Norm saxpy_golden saxpy_vector)

    printf "\n"
    printf "Scalar dot product (Double):                    %0.5f\n" (Dotp.Double.Scalar.dotp  du dv)
    printf "Hand-written Haskell SIMD dot product (Double): %0.5f\n" (Dotp.Double.Manual.dotp  du dv)
    printf "Hand-written C SIMD dot product (Double):       %0.5f\n" (Dotp.Double.CManual.dotp du dv)
    printf "C BLAS dot product (Double):                    %0.5f\n" (Dotp.Double.CBlas.dotp du dv)
    printf "Vector library dot product (Double):            %0.5f\n" (Dotp.Double.Vector.dotp  du dv)

    printf "\n"
    printf "Scalar sum (Int64):         %d\n" (Sum.Int64.Scalar.sum i64u)
    printf "Vector library sum (Int64): %d\n" (Sum.Int64.Vector.sum i64u)

    printf "\n"
    printf "Scalar rbf (Double):         %0.8f\n" (Rbf.Double.Scalar.rbf  0.001 du2 dv2)
    printf "Vector library rbf (Double): %0.8f\n" (Rbf.Double.Vector.rbf  0.001 du2 dv2)
    printf "BLAS rbf (Double):           %0.8f\n" (Rbf.Double.CManual.rbf 0.001 du2 dv2)
  where
    n :: Int
    n = 10000     -- vector length

    range :: Num a => (a, a)
    range = (-100, 100)  -- range of vector elements

    range2 :: Num a => (a, a)
    range2 = (0, 1)
