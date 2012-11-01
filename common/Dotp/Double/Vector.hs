{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Dotp.Double.Vector (
    dotp
  ) where

import Data.Primitive.Multi
import qualified Data.Vector.Unboxed as U

dotp :: U.Vector Double -> U.Vector Double -> Double
dotp v w =
    U.mfold' (+) (+) 0 $ U.mzipWith (*) (*) v w
