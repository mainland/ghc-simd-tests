{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -W -Werror #-}

module Data.Primitive.Multi.FloatX4 (
    FloatX4(..),
    Multi(..)
  ) where

import Data.Primitive
import Data.Primitive.MachDeps
import Data.Vector.Primitive as P
import Data.Vector.Unboxed as U
import GHC.Prim
import GHC.Types

import Data.Primitive.Multi
import Data.Vector.Unboxed.Packed

data FloatX4 = FX4# FloatX4#

mapFloatX4 :: (Float -> Float) -> FloatX4 -> FloatX4
mapFloatX4 f (FX4# x#) =
    let !(# a#, b#, c#, d# #) = unpackFloatX4# x#
        !(F# a'#)             = f (F# a#)
        !(F# b'#)             = f (F# b#)
        !(F# c'#)             = f (F# c#)
        !(F# d'#)             = f (F# d#)
        !fx#                  = packFloatX4# a'# b'# c'# d'#
    in
      FX4# fx#

instance Num FloatX4 where
    x + y = let !(FX4# x#) = x
                !(FX4# y#) = y
            in
              FX4# (x# `plusFloatX4#` y#)

    x - y = let !(FX4# x#) = x
                !(FX4# y#) = y
            in
              FX4# (x# `minusFloatX4#` y#)

    x * y = let !(FX4# x#) = x
                !(FX4# y#) = y
            in
              FX4# (x# `timesFloatX4#` y#)

    abs = mapFloatX4 abs

    signum = mapFloatX4 signum

    fromInteger i =
        let !(F# f#) = fromInteger i
            v#       = packFloatX4# f# f# f# f#
        in
          FX4# v#

instance Show FloatX4 where
    showsPrec _ (FX4# v#) =
        let !(# a#, b#, c#, d# #) = unpackFloatX4# v#
        in
          showString "<" . showv [F# a#, F# b#, F# c#, F# d#]
      where
        showv []       = showString "<>"
        showv [x]      = shows x . showString ">"
        showv (x : xs) = shows x . showString ", " . showv xs

instance Prim FloatX4 where
    sizeOf# _    = unI# (4*sIZEOF_FLOAT)
    alignment# _ = unI# (4*sIZEOF_FLOAT)

    indexByteArray# arr# i# =
        FX4# (indexFloatX4Array# arr# i#)

    readByteArray# arr# i# s# =
        let !(# s1#, x# #) = readFloatX4Array# arr# i# s#
        in
          (# s1#, FX4# x# #)

    writeByteArray# arr# i# (FX4# x#) s# =
        writeFloatX4Array# arr# i# x# s#

    indexOffAddr# addr# i# = FX4# (indexFloatX4OffAddr# addr# i#)

    readOffAddr# addr# i# s# =
        let !(# s1#, x# #) = readFloatX4OffAddr# addr# i# s#
        in
          (# s1#, FX4# x# #)

    writeOffAddr# addr# i# (FX4# x#) s# =
        writeFloatX4OffAddr# addr# i# x# s#

unI# :: Int -> Int#
unI# (I# n#) = n#

instance MultiPrim Float where

newtype instance Multi Float = MultiFloat FloatX4
  deriving (Prim, Num, Show)

instance PackedVector U.Vector Float where
    unsafeIndexMulti (V_Float (P.Vector i _ arr)) j =
        indexByteArrayMulti arr (i+j)

instance PackedMVector U.MVector Float where
    unsafeReadMulti (MV_Float (P.MVector i _ arr)) j =
        readByteArrayMulti arr (i+j)

    unsafeWriteMulti (MV_Float (P.MVector i _ arr)) j x =
        writeByteArrayMulti arr (i+j) x
