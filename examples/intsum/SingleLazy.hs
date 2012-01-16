{-# LANGUAGE BangPatterns #-}

module SingleLazy (
    sumFrom1To
  ) where

import Data.Int
import qualified Data.Vector.Unboxed as U

sumFrom1To :: Int64 -> Int64
sumFrom1To mx = U.foldl' (+) 0 v
  where
    v :: U.Vector Int64
    v = U.enumFromTo 1 mx
