{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -W -Werror #-}

module Data.Primitive.Multi.DoubleX2 (
    DoubleX2(..)
  ) where

import Data.Primitive
import Data.Primitive.MachDeps
import GHC.Prim
import GHC.Types

data DoubleX2 = DX2# DoubleX2#

mapDoubleX2 :: (Double -> Double) -> DoubleX2 -> DoubleX2
mapDoubleX2 f (DX2# x#) =
    let !(# a#, b# #) = unpackDoubleX2# x#
        !(D# a'#)     = f (D# a#)
        !(D# b'#)     = f (D# b#)
        !fx#          = packDoubleX2# a'# b'#
    in
      DX2# fx#

instance Num DoubleX2 where
    x + y = let !(DX2# x#) = x
                !(DX2# y#) = y
            in
              DX2# (x# `plusDoubleX2#` y#)

    x - y = let !(DX2# x#) = x
                !(DX2# y#) = y
            in
              DX2# (x# `minusDoubleX2#` y#)

    x * y = let !(DX2# x#) = x
                !(DX2# y#) = y
            in
              DX2# (x# `timesDoubleX2#` y#)

    abs = mapDoubleX2 abs

    signum = mapDoubleX2 signum

    fromInteger i =
        let !(D# f#) = fromInteger i
            v#       = packDoubleX2# f# f#
        in
          DX2# v#

instance Show DoubleX2 where
    showsPrec _ (DX2# v#) =
        let !(# a#, b# #) = unpackDoubleX2# v#
        in
          showString "<" . showv [D# a#, D# b#]
      where
        showv []       = showString "<>"
        showv [x]      = shows x . showString ">"
        showv (x : xs) = shows x . showString ", " . showv xs

instance Prim DoubleX2 where
    sizeOf# _    = unI# (2*sIZEOF_DOUBLE)
    alignment# _ = unI# (2*sIZEOF_DOUBLE)

    indexByteArray# arr# i# =
        DX2# (indexDoubleX2Array# arr# i#)

    readByteArray# arr# i# s# =
        let !(# s1#, x# #) = readDoubleX2Array# arr# i# s#
        in
          (# s1#, DX2# x# #)

    writeByteArray# arr# i# (DX2# x#) s# =
        writeDoubleX2Array# arr# i# x# s#

    indexOffAddr# addr# i# = DX2# (indexDoubleX2OffAddr# addr# i#)

    readOffAddr# addr# i# s# =
        let !(# s1#, x# #) = readDoubleX2OffAddr# addr# i# s#
        in
          (# s1#, DX2# x# #)

    writeOffAddr# addr# i# (DX2# x#) s# =
        writeDoubleX2OffAddr# addr# i# x# s#

unI# :: Int -> Int#
unI# (I# n#) = n#
