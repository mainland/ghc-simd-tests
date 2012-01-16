{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Control.Exception (evaluate)
import Control.Applicative ((<$>))
import qualified Data.Vector.Generic as G
import Data.Vector.Generic.MultiStream as MS
import qualified Data.Vector.Unboxed as U
import System.CPUTime (getCPUTime)
import System.Random (newStdGen, randomR, randomRs)

import Data.Primitive.Multi.FloatX4
import GHC.Prim
import GHC.Float

import Util

import qualified Single
import qualified Multi

main :: IO ()
main = do
    -- generate random input vectors
    g     <- newStdGen
    let a =  fst (randomR range g)
    x     <- randomU n range
    y     <- randomU n range

    -- force the evaluation of the input vectors
    evaluate a
    evaluate x
    evaluate y

    timeComp (\_ -> return $ Single.saxpy a x y) (\x -> " Single: ")
    timeComp (\_ -> return $ Multi.saxpy  a x y) (\x -> "  Multi: ")
  where
    n     = 10000000     -- vector length
    range = (-100, 100)  -- range of vector elements
