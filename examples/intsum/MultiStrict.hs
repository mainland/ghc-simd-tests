{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

#include "MachDeps.h"

module MultiStrict (
    sumFrom1To
  ) where

import qualified Data.Vector.Generic.MultiStream as MS
import qualified Data.Vector.Unboxed as U

import Data.Primitive.Multi
import GHC.Int
#if WORD_SIZE_IN_BITS < 64
import GHC.IntWord64
#endif
import GHC.Prim

sumFrom1To :: U.Vector Int64 -> Int64
sumFrom1To v = MS.foldl' (+) (+) red 0 v
  where
    red :: Multi Int64 -> Int64
    red (MultiInt64 (I64X2# iv)) =
        let !(# a, b #) = unpackInt64X2# iv
        in
#if WORD_SIZE_IN_BITS == 32
          I64# (a `plusInt64#` b)
#elif WORD_SIZE_IN_BITS == 64
          I64# (a +# b)
#endif
