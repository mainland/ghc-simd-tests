{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module MultiLazy (
    sumFrom1To
  ) where

import qualified Data.Vector.Generic.MultiStream as MS
import qualified Data.Vector.Unboxed as U

import Data.Primitive.Multi
import GHC.Float
import GHC.Prim

import Adapt

sumFrom1To :: Float -> Float
sumFrom1To mx = MS.foldl' (+) (+) red 0 v
  where
    v :: U.Vector Float
    v = U.enumFromTo 1 mx

    red :: Multi Float -> Float
    red (MultiFloat (FX4# fv)) =
        let !(# a, b, c, d #) = unpackFloatX4# fv
        in
          F# (a `plusFloat#` b `plusFloat#` c `plusFloat#` d)
