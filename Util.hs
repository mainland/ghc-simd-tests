{-# LANGUAGE ScopedTypeVariables #-}

module Util (
    randomU,
    timeComp
  ) where

import Control.Exception (evaluate)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector.Unboxed as U
import System.CPUTime (getCPUTime)
import System.Random (Random, newStdGen, randomRs)
import Text.Printf

randomU :: (U.Unbox a,Random a) => Int -> (a, a) -> IO (U.Vector a)
randomU n range = do
    gen <- newStdGen
    return $ U.fromList $ take n (randomRs range gen)

timeComp :: (Int -> IO a) -> (a -> String) -> IO ()
timeComp act sh = do
  start <- getTime
  x <- act 0
  evaluate x
  end <- getTime

  putStrLn $ sh x ++ " in " ++ secs (end - start)

{- Shamelessly stolen from criterion #-}
getTime :: IO Double
getTime = realToFrac `fmap` getPOSIXTime

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
