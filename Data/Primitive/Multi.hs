{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

#include "MachDeps.h"

module Data.Primitive.Multi (
    Multi(..),
    MultiPrim(..),
    FloatX4(..),
    DoubleX2(..),
    Int32X4(..),
    Int64X2(..),
    indexByteArrayAsMulti,
    readByteArrayAsMulti,
    writeByteArrayAsMulti,
    indexOffAddrAsMulti,
    readOffAddrAsMulti,
    writeOffAddrAsMulti,
    peekElemOffAsMulti,
    pokeElemOffAsMulti
 ) where

import Control.Monad.Primitive
import Data.Primitive
import Data.Primitive.Multi.FloatX4
import Data.Primitive.Multi.DoubleX2
import Data.Primitive.Multi.Int32X4
import Data.Primitive.Multi.Int64X2
import GHC.Prim
import GHC.Base (Int(..))
import GHC.Float (Float(..), Double(..))
import GHC.Int (Int32(..), Int64(..))
import GHC.Ptr

data family Multi a

newtype instance Multi Float = MultiFloat FloatX4
  deriving (Prim, Num, Show)

newtype instance Multi Double = MultiDouble DoubleX2
  deriving (Prim, Num, Show)

newtype instance Multi Int32 = MultiInt32 Int32X4
  deriving (Prim, Num, Show)

newtype instance Multi Int64 = MultiInt64 Int64X2
  deriving (Prim, Num, Show)

#if WORD_SIZE_IN_BITS == 32
newtype instance Multi Int = MultiInt Int32X4
  deriving (Prim, Num, Show)
#elif WORD_SIZE_IN_BITS == 64
newtype instance Multi Int = MultiInt Int64X2
  deriving (Prim, Num, Show)
#endif

class (Prim a, Prim (Multi a)) => MultiPrim a where
    -- | The number of elements of type @a@ an a @Multi a@.
    multiplicity :: Multi a -> Int
    multiplicity _ = I# (sizeOf# (undefined :: Multi a)) `quot`
                     I# (sizeOf# (undefined :: a))

    -- | Read a multi-value from the array. The offset is in elements of type
    -- @a@ rather than in elements of type @Multi a@.
    indexByteArrayAsMulti# :: ByteArray# -> Int# -> Multi a

    -- | Read a multi-value from the mutable array. The offset is in elements of
    -- type @a@ rather than in elements of type @Multi a@.
    readByteArrayAsMulti# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, Multi a #)

    -- | Write a multi-value to the mutable array. The offset is in elements of
    -- type @a@ rather than in elements of type @Multi a@.
    writeByteArrayAsMulti# :: MutableByteArray# s -> Int# -> Multi a -> State# s -> State# s

    -- | Read a multi-value from a memory position given by an address and an
    -- offset.  The memory block the address refers to must be immutable. The
    -- offset is in elements of type @a@ rather than in elements of type @Multi
    -- a@.
    indexOffAddrAsMulti# :: Addr# -> Int# -> Multi a

    -- | Read a multi-value from a memory position given by an address and an
    -- offset.  The offset is in elements of type @a@ rather than in elements of
    -- type @Multi a@.
    readOffAddrAsMulti# :: Addr# -> Int# -> State# s -> (# State# s, Multi a #)

    -- | Write a multi-value to a memory position given by an address and an
    -- offset.  The offset is in elements of type @a@ rather than in elements of
    -- type @Multi a@.
    writeOffAddrAsMulti# :: Addr# -> Int# -> Multi a -> State# s -> State# s

#define deriveMultiPrim(ty, mctr, ctr, idx_arr, rd_arr, wr_arr, idx_addr, rd_addr, wr_addr) \
instance MultiPrim ty where {                                                 \
  indexByteArrayAsMulti# arr# i# = mctr (ctr (idx_arr arr# i#))               \
; readByteArrayAsMulti#  arr# i# s# = case rd_arr arr# i# s# of               \
                        { (# s1#, x# #) -> (# s1#, mctr (ctr x#) #) }         \
; writeByteArrayAsMulti# arr# i# (mctr (ctr x#)) s# = wr_arr arr# i# x# s#    \
                                                                              \
; indexOffAddrAsMulti# addr# i# = mctr (ctr (idx_addr addr# i#))              \
; readOffAddrAsMulti#  addr# i# s# = case rd_addr addr# i# s# of              \
                        { (# s1#, x# #) -> (# s1#, mctr (ctr x#) #) }         \
; writeOffAddrAsMulti# addr# i# (mctr (ctr x#)) s# = wr_addr addr# i# x# s#   }

deriveMultiPrim(Float, MultiFloat, FX4#,
                indexFloatArrayAsFloatX4#,
                readFloatArrayAsFloatX4#,
                writeFloatArrayAsFloatX4#,
                indexFloatOffAddrAsFloatX4#,
                readFloatOffAddrAsFloatX4#,
                writeFloatOffAddrAsFloatX4#)

deriveMultiPrim(Double, MultiDouble, DX2#,
                indexDoubleArrayAsDoubleX2#,
                readDoubleArrayAsDoubleX2#,
                writeDoubleArrayAsDoubleX2#,
                indexDoubleOffAddrAsDoubleX2#,
                readDoubleOffAddrAsDoubleX2#,
                writeDoubleOffAddrAsDoubleX2#)

deriveMultiPrim(Int32, MultiInt32, I32X4#,
                indexInt32ArrayAsInt32X4#,
                readInt32ArrayAsInt32X4#,
                writeInt32ArrayAsInt32X4#,
                indexInt32OffAddrAsInt32X4#,
                readInt32OffAddrAsInt32X4#,
                writeInt32OffAddrAsInt32X4#)

deriveMultiPrim(Int64, MultiInt64, I64X2#,
                indexInt64ArrayAsInt64X2#,
                readInt64ArrayAsInt64X2#,
                writeInt64ArrayAsInt64X2#,
                indexInt64OffAddrAsInt64X2#,
                readInt64OffAddrAsInt64X2#,
                writeInt64OffAddrAsInt64X2#)

#if WORD_SIZE_IN_BITS == 32
deriveMultiPrim(Int, MultiInt, I32X4#,
                indexInt32ArrayAsInt32X4#,
                readInt32ArrayAsInt32X4#,
                writeInt32ArrayAsInt32X4#,
                indexInt32OffAddrAsInt32X4#,
                readInt32OffAddrAsInt32X4#,
                writeInt32OffAddrAsInt32X4#)
#elif WORD_SIZE_IN_BITS == 64
deriveMultiPrim(Int, MultiInt, I64X2#,
                indexInt64ArrayAsInt64X2#,
                readInt64ArrayAsInt64X2#,
                writeInt64ArrayAsInt64X2#,
                indexInt64OffAddrAsInt64X2#,
                readInt64OffAddrAsInt64X2#,
                writeInt64OffAddrAsInt64X2#)
#endif

-- | Read a primitive multi-value from the byte array. The offset is given in
-- elements of type @a@ rather than in elements of type @Multi a@.
indexByteArrayAsMulti :: MultiPrim a => ByteArray -> Int -> Multi a
{-# INLINE indexByteArrayAsMulti #-}
indexByteArrayAsMulti (ByteArray arr#) (I# i#) =
    indexByteArrayAsMulti# arr# i#

-- | Read a primitive multi-value from the byte array. The offset is given in
-- elements of type @a@ rather than in elements of type @Multi a@.
readByteArrayAsMulti :: (MultiPrim a, PrimMonad m)
                     => MutableByteArray (PrimState m) -> Int -> m (Multi a)
{-# INLINE readByteArrayAsMulti #-}
readByteArrayAsMulti (MutableByteArray arr#) (I# i#) =
    primitive (readByteArrayAsMulti# arr# i#)

-- | Write a primitive multi-value to the byte array. The offset is given in
-- elements of type @a@ rather than in elements of type @Multi a@.
writeByteArrayAsMulti :: (MultiPrim a, PrimMonad m)
                      => MutableByteArray (PrimState m) -> Int -> Multi a -> m ()
{-# INLINE writeByteArrayAsMulti #-}
writeByteArrayAsMulti (MutableByteArray arr#) (I# i#) x =
    primitive_ (writeByteArrayAsMulti# arr# i# x)

-- | Read a multi-value from a memory position given by an address and an
-- offset.  The memory block the address refers to must be immutable. The offset
-- is in elements of type @a@ rather than in elements of type @Multi a@.
indexOffAddrAsMulti :: MultiPrim a => Addr -> Int -> Multi a
{-# INLINE indexOffAddrAsMulti #-}
indexOffAddrAsMulti (Addr addr#) (I# i#) =
    indexOffAddrAsMulti# addr# i#

-- | Read a multi-value from a memory position given by an address and an
-- offset.  The offset is in elements of type @a@ rather than in elements of
-- type @Multi a@.
readOffAddrAsMulti :: (MultiPrim a, PrimMonad m) => Addr -> Int -> m (Multi a)
{-# INLINE readOffAddrAsMulti #-}
readOffAddrAsMulti (Addr addr#) (I# i#) =
    primitive (readOffAddrAsMulti# addr# i#)

-- | Write a multi-value to a memory position given by an address and an offset.
-- The offset is in elements of type @a@ rather than inelements of type @Multi
-- a@.
writeOffAddrAsMulti :: (MultiPrim a, PrimMonad m) => Addr -> Int -> Multi a -> m ()
{-# INLINE writeOffAddrAsMulti #-}
writeOffAddrAsMulti (Addr addr#) (I# i#) x =
    primitive_ (writeOffAddrAsMulti# addr# i# x)

peekElemOffAsMulti :: (MultiPrim a, PrimMonad m) => Ptr a -> Int -> m (Multi a)
peekElemOffAsMulti (Ptr a#) i = readOffAddrAsMulti (Addr a#) i

pokeElemOffAsMulti :: (MultiPrim a, PrimMonad m) => Ptr a -> Int -> Multi a -> m ()
pokeElemOffAsMulti (Ptr a#) i x = writeOffAddrAsMulti (Addr a#) i x
