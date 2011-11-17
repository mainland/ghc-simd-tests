{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Multi (
    saxpy
  ) where

import qualified Data.Vector.Generic as G
import Data.Vector.Generic.MultiStream as MS
import qualified Data.Vector.Unboxed as U

import Data.Primitive.Multi
import GHC.Prim
import GHC.Float

-- The seq is very important for efficiency! It allows the constant to be lifted
-- out of the inner computation, permitting additional fusion.
saxpy :: Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
saxpy a xs ys =
    a' `seq` MS.zipWith (\x y -> a*x + y) (\x y -> a'*x + y) xs ys
  where
    a' :: Multi Float
    a' = multireplicate a
