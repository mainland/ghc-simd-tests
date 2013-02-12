{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Dotp.Float.Vector (
    dotp
  ) where

import Data.Primitive.Multi

import qualified Vector as V

dotp :: V.Vector Float -> V.Vector Float -> Float
dotp v w =
    V.vfold (+) 0 (V.vzipWith (*) v w)
