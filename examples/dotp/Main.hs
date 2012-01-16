{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Control.Exception (evaluate)
import qualified Data.Vector.Generic as G
import Data.Vector.Generic.MultiStream as MS
import qualified Data.Vector.Unboxed as U
import System.CPUTime (getCPUTime)
import System.Random (newStdGen, randomRs)

import Data.Primitive.Multi.FloatX4
import GHC.Prim
import GHC.Float

import Util

import qualified Single
import qualified Multi
import qualified Hand

{- See http://www.haskell.org/haskellwiki/GHC/Data_Parallel_Haskell/MainTimed
 - Three versions of dot product:
 -   "Single" uses unboxed vectors of Floats
 -    "Multi" uses Data.Vector.Unboxed.PackedVector's of Floats
 -  "By-hand" is a hand-unrolled version that uses Multi Floats
 -}

main :: IO ()
main = do
    -- generate random input vectors
    u <- randomU n range
    v <- randomU n range

    -- force the evaluation of the input vectors
    evaluate u
    evaluate v

    timeComp (\_ -> return $ Single.dotp u v) (\x -> " Single: " ++ show x)
    timeComp (\_ -> return $ Multi.dotp  u v) (\x -> "  Multi: " ++ show x)
    timeComp (\_ -> return $ Hand.dotp   u v) (\x -> "By-hand: " ++ show x)
  where
    n     = 10000000     -- vector length
    range = (-100, 100)  -- range of vector elements
