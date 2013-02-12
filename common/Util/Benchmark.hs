{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Util.Benchmark
-- Copyright   : (c) Geoffrey Mainland 2011-2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Util.Benchmark (
    timeComp,
    secs,
    timeBenchmark,
    runBenchmark
  ) where

import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Primitive (sizeOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Text.Printf

timeBenchmark :: Int
              -> (a -> b)
              -> a
              -> IO (Double, Double, Double, Double)
timeBenchmark ntrials f x = do
    ts        <- replicateM ntrials (timeComp f x)
    let mean  = sum ts / fromIntegral (length ts)
    let min   = minimum ts
    let max   = maximum ts
    let sigma = sqrt (sum (map (\x -> x * x) ts) / fromIntegral (length ts) - mean*mean)
    return (mean, min, max, sigma)

runBenchmark :: Int
             -> String
             -> (a -> b)
             -> a
             -> IO ()
runBenchmark ntrials name f x = do
    (mean, min, max, sigma) <- timeBenchmark ntrials f x
    printf "%s mean %s min %s max %s var %s\n" name (secs mean) (secs min) (secs max) (secs sigma)

timeComp ::  (a -> b)
         ->  a
         ->  IO Double
timeComp f x = do
    start <- getTime
    evaluate (f x)
    end <- getTime

    return (end - start)
  where
    getTime :: IO Double
    getTime = realToFrac `fmap` getPOSIXTime
{-
    getTime = do
        t <- getCPUTime
        return $ fromInteger t * 10e-12
-}

secs :: Double -> String
secs k
    | k < 0      = '-' : secs (-k)
    | k >= 1     = k        `with` "s"
    | k >= 1e-3  = (k*1e3)  `with` "ms"
    | k >= 1e-6  = (k*1e6)  `with` "us"
    | k >= 1e-9  = (k*1e9)  `with` "ns"
    | k >= 1e-12 = (k*1e12) `with` "ps"
    | otherwise  = printf "%g s" k
     where with (t :: Double) (u :: String)
               | t >= 1e9  = printf "%.4g %s" t u
               | t >= 1e6  = printf "%.0f %s" t u
               | t >= 1e5  = printf "%.1f %s" t u
               | t >= 1e4  = printf "%.2f %s" t u
               | t >= 1e3  = printf "%.3f %s" t u
               | t >= 1e2  = printf "%.4f %s" t u
               | t >= 1e1  = printf "%.5f %s" t u
               | otherwise = printf "%.6f %s" t u
