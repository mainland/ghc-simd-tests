{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module MultiStrict (
    sumFrom1To
  ) where

import qualified Data.Vector.Generic.MultiStream as MS
import qualified Data.Vector.Unboxed as U

import Data.Primitive.Multi
import GHC.Float
import GHC.Prim

sumFrom1To :: U.Vector Float -> Float
sumFrom1To v = MS.foldl' (+) (+) red 0 v
  where
    red :: Multi Float -> Float
    red (MultiFloat (FX4# fv)) =
        let !(# a, b, c, d #) = unpackFloatX4# fv
        in
          F# (a `plusFloat#` b `plusFloat#` c `plusFloat#` d)
