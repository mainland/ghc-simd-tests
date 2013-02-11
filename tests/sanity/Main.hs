{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (evaluate)
import Data.Array.Parallel (PArray)
import Data.Array.Parallel.PArray (nf)
import Data.Int
import System.IO (hFlush, stdout)
import System.Random (newStdGen, randomR)
import Text.Printf

import qualified Vector as V

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

import qualified Rbf.Double.Blitz
import qualified Rbf.Double.Boost
import qualified Rbf.Double.CManual
import qualified Rbf.Double.CManualIntermediate
import qualified Rbf.Double.Eigen
import qualified Rbf.Double.Vector
import qualified Rbf.Double.VectorAlt1
import qualified Rbf.Double.VectorAlt2
import qualified Rbf.Double.SALT
import qualified Rbf.Double.Scalar

nTRIALS :: Int
nTRIALS = 100

main :: IO ()
main = do
    -- generate random input vectors
    g                         <-  newStdGen

    let fa :: Float           =   fst (randomR range g)
    fu :: V.Vector Float      <-  V.randomVector n range
    fv :: V.Vector Float      <-  V.randomVector n range

    du :: V.Vector Double     <-  V.randomVector n range
    dv :: V.Vector Double     <-  V.randomVector n range
    du2 :: V.Vector Double    <-  V.randomVector n range2
    dv2 :: V.Vector Double    <-  V.randomVector n range2
    let dupa :: PArray Double =   V.toPArray du
    let dvpa :: PArray Double =   V.toPArray dv

    i64u :: V.Vector Int64    <-  V.randomVector n range

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
    printf "Scalar rbf (Double):              %0.8f\n" (Rbf.Double.Scalar.rbf     0.001 du2 dv2)
    printf "Vector library rbf (Double):      %0.8f\n" (Rbf.Double.Vector.rbf     0.001 du2 dv2)
    printf "Vector library rbf2 (Double):     %0.8f\n" (Rbf.Double.VectorAlt1.rbf 0.001 du2 dv2)
    printf "Vector library rbf3 (Double):     %0.8f\n" (Rbf.Double.VectorAlt2.rbf 0.001 du2 dv2)
    printf "BLAS rbf (Double):                %0.8f\n" (Rbf.Double.CManual.rbf    0.001 du2 dv2)
    printf "BLAS rbf (intermediate) (Double): %0.8f\n" (Rbf.Double.CManualIntermediate.rbf 0.001 du2 dv2)
    printf "Blitz rbf:                        %0.8f\n" (Rbf.Double.Blitz.rbf  0.001 du2 dv2)
    printf "Boost uBLAS rbf:                  %0.8f\n" (Rbf.Double.Boost.rbf  0.001 du2 dv2)
    printf "Eigen rbf:                        %0.8f\n" (Rbf.Double.Eigen.rbf  0.001 du2 dv2)
    printf "Eigen rbf2:                       %0.8f\n" (Rbf.Double.Eigen.rbf2 0.001 du2 dv2)
    printf "SALT rbf:                         %0.8f\n" (Rbf.Double.SALT.rbf   0.001 du2 dv2)
  where
    n :: Int
    n = 10000     -- vector length

    range :: Num a => (a, a)
    range = (-100, 100)  -- range of vector elements

    range2 :: Num a => (a, a)
    range2 = (0, 1)
