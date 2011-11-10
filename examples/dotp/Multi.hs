{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Multi (
    dotp
  ) where

import qualified Data.Vector.Generic as G
import Data.Vector.Generic.MultiStream as MS
import qualified Data.Vector.Unboxed as U

import Data.Primitive.Multi.FloatX4
import GHC.Prim
import GHC.Float

dotp :: U.Vector Float -> U.Vector Float -> Float
dotp v w =
    MS.foldl' (+) (+) red 0 $ MS.zipWith (*) (*) v w
  where
    {-# INLINE red #-}
    red :: Multi Float -> Float
    red (MultiFloat (FX4# fv)) =
        let !(# a, b, c, d #) = unpackFloatX4# fv
        in
          F# (a `plusFloat#` b `plusFloat#` c `plusFloat#` d)
