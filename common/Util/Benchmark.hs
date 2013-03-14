{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Util.Benchmark
-- Copyright   : (c) Geoffrey Mainland 2011-2013
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Util.Benchmark (
    Clock(..),
    timeBenchmark,
    runBenchmark,
    secs
  ) where

import Control.Applicative
import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Primitive (sizeOf)
import Text.Printf

import Util.Time

data Clock = WallClock
           | CPUClock

wARMUP_TRIALS :: Int
wARMUP_TRIALS = 3

timeBenchmark :: Clock
              -> Int
              -> Maybe Double
              -> (a -> b)
              -> a
              -> IO (Double, Double, Double, Double)
-- Perform @ntrials@ trials.
timeBenchmark clock ntrials Nothing f x = do
    ts        <- drop wARMUP_TRIALS <$> replicateM (wARMUP_TRIALS+ntrials) (timeComp f x)
    let mean  =  sum ts / fromIntegral (length ts)
    let min   =  minimum ts
    let max   =  maximum ts
    let sigma =  variance ts mean
    return (mean, min, max, sigma)
  where
    timeComp :: (a -> b)
             -> a
             -> IO Double
    {-# NOINLINE timeComp #-}
    timeComp f x = do
        start <- getTime
        evaluate $ f x
        end   <- getTime
        return $ end - start

    getTime = case clock of
                WallClock -> getWallTime
                CPUClock  -> getCPUTime

-- Perform at least @ntrials@ trials and at least enough trials so that combined
-- they take at least @minTime@ seconds.
timeBenchmark clock ntrials (Just minTime) f x = do
    minTimeTrials <- calcMinTrials minTime f x
    replicateM_ wARMUP_TRIALS $ evalNf f x
    t <- timeComp (max ntrials minTimeTrials) f x
    return (t, t, t, 0)
  where
    timeComp :: Int
             -> (a -> b)
             -> a
             -> IO Double
    {-# NOINLINE timeComp #-}
    timeComp ntrials f x = do
        start   <- getTime
        replicateM_ ntrials $ evalNf f x
        end     <- getTime
        return $ (end - start) / fromIntegral ntrials

    getTime = case clock of
                WallClock -> getWallTime
                CPUClock  -> getCPUTime

variance :: [Double] -> Double -> Double
variance xs mean =
    if temp == 0 || length xs <= 1
       then 0
       else sqrt $ temp / (fromIntegral (length xs) - 1)
  where
    temp = sum [square (x-mean) | x <- xs]

    square x = x * x

-- Calculate the minimum number of trials needed for computation to take at
-- least @'t'@ seconds.
calcMinTrials :: Double
              -> (a -> b)
              -> a
              -> IO Int
calcMinTrials minTime f x = do
    start <- getCPUTime
    loop start 0
  where
    loop :: Double -> Int -> IO Int
    loop !start !n = do
        evalNf f x
        now <- getCPUTime
        if now - start > minTime
          then return n
          else loop start (n+1)

evalNf :: (a -> b) -> a -> IO ()
{-# NOINLINE evalNf #-}
evalNf f x = evaluate (f x) >> return ()

replicateM_ :: Int -> IO a -> IO ()
replicateM_ 0  m = return ()
replicateM_ !n m = do  m
                       replicateM_ (n-1) m

runBenchmark :: Clock
             -> Int
             -> String
             -> (a -> b)
             -> a
             -> IO ()
runBenchmark clock ntrials name f x = do
    (mean, min, max, sigma) <- timeBenchmark clock ntrials Nothing f x
    printf "%s mean %s min %s max %s var %s\n" name (secs mean) (secs min) (secs max) (secs sigma)

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
