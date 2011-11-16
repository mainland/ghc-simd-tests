{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -W -Werror #-}

module Data.Primitive.Multi.Int32X4 (
    Int32X4(..)
  ) where

import Data.Primitive
import Data.Primitive.MachDeps
import GHC.Int
import GHC.Prim
import GHC.Types

data Int32X4 = I32X4# Int32X4#

mapInt32X4 :: (Int32 -> Int32) -> Int32X4 -> Int32X4
mapInt32X4 f (I32X4# x#) =
    let !(# a#, b#, c#, d# #) = unpackInt32X4# x#
        !(I32# a'#)           = f (I32# a#)
        !(I32# b'#)           = f (I32# b#)
        !(I32# c'#)           = f (I32# c#)
        !(I32# d'#)           = f (I32# d#)
        !fx#                  = packInt32X4# a'# b'# c'# d'#
    in
      I32X4# fx#

instance Num Int32X4 where
    x + y = let !(I32X4# x#) = x
                !(I32X4# y#) = y
            in
              I32X4# (x# `plusInt32X4#` y#)

    x - y = let !(I32X4# x#) = x
                !(I32X4# y#) = y
            in
              I32X4# (x# `minusInt32X4#` y#)

    x * y = let !(I32X4# x#) = x
                !(I32X4# y#) = y
            in
              I32X4# (x# `timesInt32X4#` y#)

    abs = mapInt32X4 abs

    signum = mapInt32X4 signum

    fromInteger i =
        let !(I32# n#) = fromInteger i
            v#         = packInt32X4# n# n# n# n#
        in
          I32X4# v#

instance Show Int32X4 where
    showsPrec _ (I32X4# v#) =
        let !(# a#, b#, c#, d# #) = unpackInt32X4# v#
        in
          showString "<" . showv [I32# a#, I32# b#, I32# c#, I32# d#]
      where
        showv []       = showString "<>"
        showv [x]      = shows x . showString ">"
        showv (x : xs) = shows x . showString ", " . showv xs

instance Prim Int32X4 where
    sizeOf# _    = unI# (4*sIZEOF_INT32)
    alignment# _ = unI# (4*sIZEOF_INT32)

    indexByteArray# arr# i# =
        I32X4# (indexInt32X4Array# arr# i#)

    readByteArray# arr# i# s# =
        let !(# s1#, x# #) = readInt32X4Array# arr# i# s#
        in
          (# s1#, I32X4# x# #)

    writeByteArray# arr# i# (I32X4# x#) s# =
        writeInt32X4Array# arr# i# x# s#

    indexOffAddr# addr# i# = I32X4# (indexInt32X4OffAddr# addr# i#)

    readOffAddr# addr# i# s# =
        let !(# s1#, x# #) = readInt32X4OffAddr# addr# i# s#
        in
          (# s1#, I32X4# x# #)

    writeOffAddr# addr# i# (I32X4# x#) s# =
        writeInt32X4OffAddr# addr# i# x# s#

unI# :: Int -> Int#
unI# (I# n#) = n#
