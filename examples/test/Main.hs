{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
-- {-# OPTIONS_GHC -W -Werror #-}

#include "MachDeps.h"

module Main where

import Prelude hiding (map, zipWith, foldl)

import Data.Primitive.Multi
import Data.Primitive.Multi.FloatX4
import Data.Primitive.Multi.DoubleX2
import Data.Primitive.Multi.Int32X4
import Data.Primitive.Multi.Int64X2
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Packed
import qualified Data.Vector.Generic as G1
import qualified Data.Vector.Generic.New as GN
import qualified Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Generic.MultiStream as G
import qualified Data.Vector.Fusion.MultiStream.Monadic as MS
import GHC.Float
import GHC.Int
import GHC.Prim
import GHC.Types

#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif

import Util

main :: IO ()
main = do
    timeComp (\_ -> return $ sumFloat vf)
       (\x -> "       Float: " ++ show x)
    timeComp (\_ -> return $ sumMultiFloat vf)
       (\x -> " Multi Float: " ++ show x)

    timeComp (\_ -> return $ sumDouble vd)
       (\x -> "      Double: " ++ show x)
    timeComp (\_ -> return $ sumMultiDouble vd)
       (\x -> "Multi Double: " ++ show x)

    timeComp (\_ -> return $ sumInt32 vi32)
       (\x -> "       Int32: " ++ show x)
    timeComp (\_ -> return $ sumMultiInt32 vi32)
       (\x -> " Multi Int32: " ++ show x)

    timeComp (\_ -> return $ sumInt64 vi64)
       (\x -> "       Int64: " ++ show x)
    timeComp (\_ -> return $ sumMultiInt64 vi64)
       (\x -> " Multi Int64: " ++ show x)
  where
    mx :: Num a => a
    mx = 10000000

    vf :: U.Vector Float
    !vf = U.enumFromTo 1 mx

    vd :: U.Vector Double
    !vd = U.enumFromTo 1 mx

    vi32 :: U.Vector Int32
    !vi32 = U.enumFromTo 1 mx

    vi64 :: U.Vector Int64
    !vi64 = U.enumFromTo 1 mx

sumFloat :: U.Vector Float -> Float
sumFloat v = G1.foldl' (+) 0 v

sumMultiFloat :: U.Vector Float -> Float
sumMultiFloat v = G.foldl' (+) (+) red 0 v
  where
    red :: Multi Float -> Float
    red (MultiFloat (FX4# fv)) =
        let !(# a, b, c, d #) = unpackFloatX4# fv
        in
          F# (a `plusFloat#` b `plusFloat#` c `plusFloat#` d)

sumDouble :: U.Vector Double -> Double
sumDouble v = G1.foldl' (+) 0 v

sumMultiDouble :: U.Vector Double -> Double
sumMultiDouble v = G.foldl' (+) (+) red 0 v
  where
    red :: Multi Double -> Double
    red (MultiDouble (DX2# fv)) =
        let !(# a, b #) = unpackDoubleX2# fv
        in
          D# (a +## b)

sumInt32 :: U.Vector Int32 -> Int32
sumInt32 v = G1.foldl' (+) 0 v

sumMultiInt32 :: U.Vector Int32 -> Int32
sumMultiInt32 v = G.foldl' (+) (+) red 0 v
  where
    red :: Multi Int32 -> Int32
    red (MultiInt32 (I32X4# v)) =
        let !(# a, b, c, d #) = unpackInt32X4# v
        in
          I32# (a +# b +# c +# d)

sumInt64 :: U.Vector Int64 -> Int64
sumInt64 v = G1.foldl' (+) 0 v

sumMultiInt64 :: U.Vector Int64 -> Int64
sumMultiInt64 v = G.foldl' (+) (+) red 0 v
  where
    red :: Multi Int64 -> Int64
    red (MultiInt64 (I64X2# v)) =
        let !(# a, b #) = unpackInt64X2# v
        in
#if WORD_SIZE_IN_BITS < 64
          I64# (a `plusInt64#` b)
#else
          I64# (a +# b)
#endif
