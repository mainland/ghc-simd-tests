{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (evaluate)
import Foreign (sizeOf)
import System.IO (hFlush, stdout)
import Text.Printf

import qualified Vector as V

import Util.Benchmark

import qualified Sum.Double.Scalar
import qualified Sum.Double.CScalar
import qualified Sum.Double.Manual
import qualified Sum.Double.CManual
import qualified Sum.Double.Vector

import qualified Dotp.Double.Scalar
import qualified Dotp.Double.CScalar
import qualified Dotp.Double.Manual
import qualified Dotp.Double.CManual
import qualified Dotp.Double.CBlas
import qualified Dotp.Double.Vector

import qualified Rbf.Double.CManual
import qualified Rbf.Double.CManualIntermediate
import qualified Rbf.Double.Vector
import qualified Rbf.Double.VectorAlt
import qualified Rbf.Double.Scalar

nTRIALS :: Int
nTRIALS = 100

main :: IO ()
main =
    mapM_ runN (map (round . (2**)) [12..24 :: Float])
  where
    runN :: Int -> IO ()
    runN n = do
        -- generate random input vectors
        fu :: V.Vector Double <-  V.randomVector n range
        fv :: V.Vector Double <-  V.randomVector n range

        fu2 :: V.Vector Double <-  V.randomVector n range2
        fv2 :: V.Vector Double <-  V.randomVector n range2

        -- putStr "Generating random vectors..."
        hFlush stdout
        evaluate fu
        evaluate fv
        evaluate fu2
        evaluate fv2
        -- putStrLn "done."
        hFlush stdout

        runOne "sum" "scalar"      n Sum.Double.Scalar.sum  fu
        runOne "sum" "cscalar"     n Sum.Double.CScalar.sum fu
        runOne "sum" "manual"      n Sum.Double.Manual.sum  fu
        runOne "sum" "cmanual"     n Sum.Double.CManual.sum fu
        runOne "sum" "vector"      n Sum.Double.Vector.sum  fu

        runOne "dotp" "scalar"      n (uncurry Dotp.Double.Scalar.dotp)  (fu, fv)
        runOne "dotp" "cscalar"     n (uncurry Dotp.Double.CScalar.dotp) (fu, fv)
        runOne "dotp" "manual"      n (uncurry Dotp.Double.Manual.dotp)  (fu, fv)
        runOne "dotp" "cmanual"     n (uncurry Dotp.Double.CManual.dotp) (fu, fv)
        runOne "dotp" "cblas"       n (uncurry Dotp.Double.CBlas.dotp)   (fu, fv)
        runOne "dotp" "vector"      n (uncurry Dotp.Double.Vector.dotp)  (fu, fv)

        runOne "rbf" "scalar"       n (uncurry (Rbf.Double.Scalar.rbf nu))  (fu2, fv2)
        runOne "rbf" "vector"       n (uncurry (Rbf.Double.Vector.rbf nu))  (fu2, fv2)
        runOne "rbf" "vector_alt"   n (uncurry (Rbf.Double.VectorAlt.rbf nu))  (fu2, fv2)
        runOne "rbf" "cmanual"      n (uncurry (Rbf.Double.CManual.rbf nu)) (fu2, fv2)
        runOne "rbf" "cmanual_int"  n (uncurry (Rbf.Double.CManualIntermediate.rbf nu)) (fu2, fv2)

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

    range2 :: Num a => (a, a)
    range2 = (0, 1)  -- range of vector elements

    nu :: Double
    nu = 0.001
