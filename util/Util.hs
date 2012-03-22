{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util (
    randomU,
    timeComp,
    secs,
    timeBenchmark,
    runBenchmark,
    unsafeFloatUVectorToPtr
  ) where

import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Primitive (sizeOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import Foreign.C
import Foreign.Ptr
import GHC.Ptr
--import System.CPUTime (getCPUTime)
import System.Random (Random, newStdGen, randomRs)
import Text.Printf

randomU :: (U.Unbox a,Random a) => Int -> (a, a) -> IO (U.Vector a)
randomU n range = do
    gen <- newStdGen
    return $ U.fromList $ take n (randomRs range gen)

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

unsafeFloatUVectorToPtr :: U.Vector Float -> (Ptr CFloat, CInt)
{-# INLINE unsafeFloatUVectorToPtr #-}
unsafeFloatUVectorToPtr (U.V_Float (P.Vector off len arr)) =
    (p, fromIntegral (fromIntegral (len - off)))
  where
    p :: Ptr CFloat
    p = case byteArrayContents arr `plusAddr` off*sz of
          Addr a -> Ptr a

    sz :: Int
    sz = sizeOf (undefined :: Float)
