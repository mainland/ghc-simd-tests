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

import Data.Primitive.Multi
import GHC.Prim
import GHC.Float

dotp :: U.Vector Float -> U.Vector Float -> Float
dotp v w =
    MS.foldl (+) (+) (multifold (+) 0) 0 $ MS.zipWith (*) (*) v w
