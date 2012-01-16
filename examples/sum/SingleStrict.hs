{-# LANGUAGE BangPatterns #-}

module SingleStrict (
    sumFrom1To
  ) where

import qualified Data.Vector.Unboxed as U

sumFrom1To :: U.Vector Float -> Float
sumFrom1To v = U.foldl' (+) 0 v
