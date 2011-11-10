{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Primitive.Multi (
    Multi,
    MultiPrim(..),
    indexByteArrayMulti#,
    readByteArrayMulti#,
    writeByteArrayMulti#,
    indexByteArrayMulti,
    readByteArrayMulti,
    writeByteArrayMulti
 ) where

import Control.Monad.Primitive
import Data.Primitive

import GHC.Prim
import GHC.Types

data family Multi a

class (Prim a, Prim (Multi a)) => MultiPrim a where
    multiplicity :: Multi a -> Int
    multiplicity _ = I# (sizeOf# (undefined :: Multi a)) `quot`
                     I# (sizeOf# (undefined :: a))

-- | Read a value from the array. The offset is in elements of type @a@ rather
-- than in bytes or elements of type @Multi a@.
indexByteArrayMulti# :: forall a . (MultiPrim a)
                     => ByteArray#
                     -> Int#
                     -> Multi a
{-# INLINE indexByteArrayMulti# #-}
indexByteArrayMulti# arr i =
    let !addr1         = byteArrayContents# arr
        !i'            = i *# sizeOf# (undefined :: a)
        !addr2         = addr1 `plusAddr#` i'
        !v             = indexOffAddr# addr2 0#
    in
      v

-- | Read a value from the mutable array. The offset is in elements of type @a@
-- rather than in bytes or elements of type @Multi a@.
readByteArrayMulti# :: forall a s . (MultiPrim a)
                    => MutableByteArray# s
                    -> Int#
                    -> State# s
                    -> (# State# s, Multi a #)
{-# INLINE readByteArrayMulti# #-}
readByteArrayMulti# barr i s1 =
    let !(# s2, arr #) = unsafeFreezeByteArray# barr s1
        !addr1         = byteArrayContents# arr
        !i'            = i *# sizeOf# (undefined :: a)
        !addr2         = addr1 `plusAddr#` i'
        !(# s3, v #)   = readOffAddr# addr2 0# s2
    in
      (# s3, v #)

-- | Write a value to the mutable array. The offset is in elements of type @a@
-- rather than in bytes or elements of type @Multi a@.
writeByteArrayMulti# :: forall a s . (MultiPrim a)
                     => MutableByteArray# s
                     -> Int#
                     -> Multi a
                     -> State# s
                     -> State# s
{-# INLINE writeByteArrayMulti# #-}
writeByteArrayMulti# barr i x s1 =
    let !(# s2, arr #) = unsafeFreezeByteArray# barr s1
        !addr1         = byteArrayContents# arr
        !i'            = i *# sizeOf# (undefined :: a)
        !addr2         = addr1 `plusAddr#` i'
        !s3            = writeOffAddr# addr2 0# x s2
    in
      s3

-- | Read a primitive value from the byte array. The offset is given in elements
-- of type @a@ rather than in bytes or elements of type @Multi a@.
indexByteArrayMulti :: (MultiPrim a)
                    => ByteArray
                    -> Int
                    -> Multi a
{-# INLINE indexByteArrayMulti #-}
indexByteArrayMulti (ByteArray arr#) (I# i#) =
    indexByteArrayMulti# arr# i#

-- | Read a primitive value from the byte array. The offset is given in elements
-- of type @a@ rather than in bytes or elements of type @Multi a@.
readByteArrayMulti :: (MultiPrim a, PrimMonad m)
                   => MutableByteArray (PrimState m)
                   -> Int
                   -> m (Multi a)
{-# INLINE readByteArrayMulti #-}
readByteArrayMulti (MutableByteArray arr#) (I# i#) =
    primitive (readByteArrayMulti# arr# i#)

-- | Write a primitive value to the byte array. The offset is given in
-- elements of type @a@ rather than in bytes or elements of type @Multi a@.
writeByteArrayMulti :: (MultiPrim a, PrimMonad m)
                    => MutableByteArray (PrimState m)
                    -> Int
                    -> Multi a
                    -> m ()
{-# INLINE writeByteArrayMulti #-}
writeByteArrayMulti (MutableByteArray arr#) (I# i#) x =
    primitive_ (writeByteArrayMulti# arr# i# x)
