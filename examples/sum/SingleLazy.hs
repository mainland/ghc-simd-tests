{-# LANGUAGE BangPatterns #-}

module SingleLazy (
    sumFrom1To
  ) where

import qualified Data.Vector.Unboxed as U

sumFrom1To :: Float -> Float
sumFrom1To mx = U.foldl' (+) 0 v
  where
    v :: U.Vector Float
    v = U.enumFromTo 1 mx
