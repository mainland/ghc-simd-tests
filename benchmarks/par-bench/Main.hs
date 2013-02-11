{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent (getNumCapabilities,
                           setNumCapabilities)
import Control.Exception (evaluate)
import Data.Array.Parallel (PArray)
import Data.Array.Parallel.PArray (nf, fromUArray)
import qualified Data.Vector.Unboxed as U
import System.IO (hFlush, stdout)
import Text.Printf

import Util.Random
import Util.Benchmark

import qualified Dotp.Double.Scalar
import qualified Dotp.Double.Manual
import qualified Dotp.Double.CManual
import qualified Dotp.Double.Vector
import qualified Dotp.Double.Dph
import qualified Dotp.Double.DphMulti
import qualified Dotp.Double.DphPA

nTRIALS :: Int
nTRIALS = 100

main :: IO ()
main = do
    m_max <- getNumCapabilities
    let ms | m_max == 1 = [1]
           | otherwise  = m_max : m_max-1 : downBy2 m_max
    mapM_ runN [(round (2**n), m) | n <- [24 :: Double], m <- ms]
  where
    runN :: (Int, Int) -> IO ()
    runN (n, m) = do
        -- generate random input vectors
        du :: U.Vector Double      <-  randomU n range
        dv :: U.Vector Double      <-  randomU n range
        let dupa :: PArray Double  =   fromUArray du
        let dvpa :: PArray Double  =   fromUArray dv

        -- putStr "Generating random vectors..."
        hFlush stdout
        evaluate du
        evaluate dv
        -- putStr "Converting to parallel arrays..."
        evaluate $ nf dupa
        evaluate $ nf dvpa
        -- putStrLn "done."
        hFlush stdout

        setNumCapabilities m

        runOne "dotp" "scalar"      n m (uncurry Dotp.Double.Scalar.dotp)   (du, dv)
        runOne "dotp" "manual"      n m (uncurry Dotp.Double.Manual.dotp)   (du, dv)
        runOne "dotp" "cmanual"     n m (uncurry Dotp.Double.CManual.dotp)  (du, dv)
        runOne "dotp" "vector"      n m (uncurry Dotp.Double.Vector.dotp)   (du, dv)
        runOne "dotp" "dph"         n m (uncurry Dotp.Double.Dph.dotp)      (dupa, dvpa)
        runOne "dotp" "dphpa"       n m (uncurry Dotp.Double.DphPA.dotp)    (dupa, dvpa)
        runOne "dotp" "dphmulti"    n m (uncurry Dotp.Double.DphMulti.dotp) (dupa, dvpa)

    runOne  ::  String
            ->  String
            ->  Int
            ->  Int
            ->  (a -> b)
            ->  a
            ->  IO ()
    runOne func variant n m f x = do
        (mean, max, min, sigma) <- timeBenchmark nTRIALS f x
        printf "%s,%s,%d,%d,%02f,%02f,%02f,%02f\n" func variant n m mean min max sigma

    range :: Num a => (a, a)
    range = (-100, 100)  -- range of vector elements

    downBy2 :: Int -> [Int]
    downBy2 0             = []
    downBy2 1             = []
    downBy2 i | i < 8     = i-1 : downBy2 (i-1)
              | otherwise = i `div` 2 : downBy2 (i `div` 2)
