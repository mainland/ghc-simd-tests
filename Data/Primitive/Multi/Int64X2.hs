{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -W -Werror #-}

module Data.Primitive.Multi.Int64X2 (
    Int64X2(..)
  ) where

import Data.Primitive
import Data.Primitive.MachDeps
import GHC.Int
import GHC.Prim
import GHC.Types

data Int64X2 = I64X2# Int64X2#

mapInt64X2 :: (Int64 -> Int64) -> Int64X2 -> Int64X2
mapInt64X2 f (I64X2# x#) =
    let !(# a#, b# #) = unpackInt64X2# x#
        !(I64# a'#)   = f (I64# a#)
        !(I64# b'#)   = f (I64# b#)
        !fx#          = packInt64X2# a'# b'#
    in
      I64X2# fx#

instance Num Int64X2 where
    x + y = let !(I64X2# x#) = x
                !(I64X2# y#) = y
            in
              I64X2# (x# `plusInt64X2#` y#)

    x - y = let !(I64X2# x#) = x
                !(I64X2# y#) = y
            in
              I64X2# (x# `minusInt64X2#` y#)

    x * y = let !(I64X2# x#) = x
                !(I64X2# y#) = y
            in
              I64X2# (x# `timesInt64X2#` y#)

    abs = mapInt64X2 abs

    signum = mapInt64X2 signum

    fromInteger i =
        let !(I64# n#) = fromInteger i
            v#         = packInt64X2# n# n#
        in
          I64X2# v#

instance Show Int64X2 where
    showsPrec _ (I64X2# v#) =
        let !(# a#, b# #) = unpackInt64X2# v#
        in
          showString "<" . showv [I64# a#, I64# b#]
      where
        showv []       = showString "<>"
        showv [x]      = shows x . showString ">"
        showv (x : xs) = shows x . showString ", " . showv xs

instance Prim Int64X2 where
    sizeOf# _    = unI# (2*sIZEOF_INT64)
    alignment# _ = unI# (2*sIZEOF_INT64)

    indexByteArray# arr# i# =
        I64X2# (indexInt64X2Array# arr# i#)

    readByteArray# arr# i# s# =
        let !(# s1#, x# #) = readInt64X2Array# arr# i# s#
        in
          (# s1#, I64X2# x# #)

    writeByteArray# arr# i# (I64X2# x#) s# =
        writeInt64X2Array# arr# i# x# s#

    indexOffAddr# addr# i# = I64X2# (indexInt64X2OffAddr# addr# i#)

    readOffAddr# addr# i# s# =
        let !(# s1#, x# #) = readInt64X2OffAddr# addr# i# s#
        in
          (# s1#, I64X2# x# #)

    writeOffAddr# addr# i# (I64X2# x#) s# =
        writeInt64X2OffAddr# addr# i# x# s#

unI# :: Int -> Int#
unI# (I# n#) = n#
