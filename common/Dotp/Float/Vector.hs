{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Dotp.Float.Vector (
    dotp
  ) where

import Data.Primitive.Multi
import qualified Data.Vector.Unboxed as U

dotp :: U.Vector Float -> U.Vector Float -> Float
dotp v w =
    U.mfold' (+) (+) 0 $ U.mzipWith (*) (*) v w
