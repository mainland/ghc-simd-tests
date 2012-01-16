{-# LANGUAGE BangPatterns #-}

module SingleStrict (
    sumFrom1To
  ) where

import Data.Int
import qualified Data.Vector.Unboxed as U

sumFrom1To :: U.Vector Int64 -> Int64
sumFrom1To v = U.foldl' (+) 0 v
