{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (evaluate)
import qualified Data.Vector.Unboxed as U
import System.IO (hFlush, stdout)
import Text.Printf

import Util

import qualified Sum.Float.Scalar
import qualified Sum.Float.CScalar
import qualified Sum.Float.Manual
import qualified Sum.Float.CManual
import qualified Sum.Float.Vector
import qualified Sum.Float.Multivector

import qualified Dotp.Float.Scalar
import qualified Dotp.Float.CScalar
import qualified Dotp.Float.Manual
import qualified Dotp.Float.CManual
import qualified Dotp.Float.Vector
import qualified Dotp.Float.Multivector
import qualified Dotp.Float.VectorAlt1
import qualified Dotp.Float.VectorAlt2
import qualified Dotp.Float.VectorAlt3
import qualified Dotp.Float.VectorAlt4

nTRIALS :: Int
nTRIALS = 100

main :: IO ()
main =
    mapM_ runN (map (round . (2**)) [10..24 :: Float])
  where
    runN :: Int -> IO ()
    runN n = do
        -- generate random input vectors
        fu :: U.Vector Float      <-  randomU n range
        fv :: U.Vector Float      <-  randomU n range

        -- putStr "Generating random vectors..."
        hFlush stdout
        evaluate fu
        evaluate fv
        -- putStrLn "done."
        hFlush stdout

        runOne "sum" "scalar"      n Sum.Float.Scalar.sum fu
        runOne "sum" "cscalar"     n Sum.Float.CScalar.sum fu
        runOne "sum" "manual"      n Sum.Float.Manual.sum fu
        runOne "sum" "cmanual"     n Sum.Float.CManual.sum fu
        runOne "sum" "multivector" n Sum.Float.Multivector.sum fu
        runOne "sum" "vector"      n Sum.Float.Vector.sum fu

        runOne "dotp" "scalar"      n (uncurry Dotp.Float.Scalar.dotp) (fu, fv)
        runOne "dotp" "cscalar"     n (uncurry Dotp.Float.CScalar.dotp) (fu, fv)
        runOne "dotp" "manual"      n (uncurry Dotp.Float.Manual.dotp) (fu, fv)
        runOne "dotp" "cmanual"     n (uncurry Dotp.Float.CManual.dotp) (fu, fv)
        runOne "dotp" "multivector" n (uncurry Dotp.Float.Multivector.dotp) (fu, fv)
        runOne "dotp" "vector"      n (uncurry Dotp.Float.Vector.dotp) (fu, fv)
        runOne "dotp" "vectoralt1"  n (uncurry Dotp.Float.VectorAlt1.dotp) (fu, fv)
        runOne "dotp" "vectoralt2"  n (uncurry Dotp.Float.VectorAlt2.dotp) (fu, fv)
        runOne "dotp" "vectoralt3"  n (uncurry Dotp.Float.VectorAlt3.dotp) (fu, fv)
        runOne "dotp" "vectoralt4"  n (uncurry Dotp.Float.VectorAlt4.dotp) (fu, fv)

    runOne  ::  String
            ->  String
            ->  Int
            ->  (a -> b)
            ->  a
            ->  IO ()
    runOne func variant n f x = do
        (mean, max, min, sigma) <- timeBenchmark nTRIALS f x
        printf "%s,%s,%d,%02f,%02f,%02f,%02f\n" func variant n mean min max sigma

    range :: Num a => (a, a)
    range = (-100, 100)  -- range of vector elements
