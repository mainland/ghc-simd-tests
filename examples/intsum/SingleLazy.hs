{-# LANGUAGE BangPatterns #-}

module SingleLazy (
    sumFrom1To
  ) where

import Data.Int
import qualified Data.Vector.Unboxed as U

sumFrom1To :: Int32 -> Int32
sumFrom1To mx = U.foldl' (+) 0 v
  where
    v :: U.Vector Int32
    v = U.enumFromTo 1 mx
