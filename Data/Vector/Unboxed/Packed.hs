{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -W -Werror #-}

#include "MachDeps.h"

module Data.Vector.Unboxed.Packed where

import Control.Monad.Primitive
import Data.Vector.Generic as V
import Data.Vector.Generic.Mutable as M
import Data.Vector.Primitive as P
import Data.Vector.Unboxed as U
import GHC.Int

import Data.Primitive.Multi
import Data.Primitive.Multi.FloatX4 ()
import Data.Primitive.Multi.DoubleX2 ()
import Data.Primitive.Multi.Int32X4 ()
import Data.Primitive.Multi.Int64X2 ()

class (MultiPrim a, V.Vector v a, PackedMVector (Mutable v) a) => PackedVector v a where
    unsafeIndexAsMulti :: v a -> Int -> Multi a

class (MultiPrim a, M.MVector v a) => PackedMVector v a where
    unsafeReadAsMulti  :: (PrimMonad m) => v (PrimState m) a -> Int -> m (Multi a)
    unsafeWriteAsMulti :: (PrimMonad m) => v (PrimState m) a -> Int -> Multi a -> m ()

instance PackedVector U.Vector Float where
    unsafeIndexAsMulti (V_Float (P.Vector i _ arr)) j =
        indexByteArrayAsMulti arr (i+j)

instance PackedMVector U.MVector Float where
    unsafeReadAsMulti (MV_Float (P.MVector i _ arr)) j =
        readByteArrayAsMulti arr (i+j)

    unsafeWriteAsMulti (MV_Float (P.MVector i _ arr)) j x =
        writeByteArrayAsMulti arr (i+j) x

instance PackedVector U.Vector Double where
    unsafeIndexAsMulti (V_Double (P.Vector i _ arr)) j =
        indexByteArrayAsMulti arr (i+j)

instance PackedMVector U.MVector Double where
    unsafeReadAsMulti (MV_Double (P.MVector i _ arr)) j =
        readByteArrayAsMulti arr (i+j)

    unsafeWriteAsMulti (MV_Double (P.MVector i _ arr)) j x =
        writeByteArrayAsMulti arr (i+j) x

instance PackedVector U.Vector Int32 where
    unsafeIndexAsMulti (V_Int32 (P.Vector i _ arr)) j =
        indexByteArrayAsMulti arr (i+j)

instance PackedMVector U.MVector Int32 where
    unsafeReadAsMulti (MV_Int32 (P.MVector i _ arr)) j =
        readByteArrayAsMulti arr (i+j)

    unsafeWriteAsMulti (MV_Int32 (P.MVector i _ arr)) j x =
        writeByteArrayAsMulti arr (i+j) x

instance PackedVector U.Vector Int64 where
    unsafeIndexAsMulti (V_Int64 (P.Vector i _ arr)) j =
        indexByteArrayAsMulti arr (i+j)

instance PackedMVector U.MVector Int64 where
    unsafeReadAsMulti (MV_Int64 (P.MVector i _ arr)) j =
        readByteArrayAsMulti arr (i+j)

    unsafeWriteAsMulti (MV_Int64 (P.MVector i _ arr)) j x =
        writeByteArrayAsMulti arr (i+j) x

#if WORD_SIZE_IN_BITS == 32 || WORD_SIZE_IN_BITS == 64
instance PackedVector U.Vector Int where
    unsafeIndexAsMulti (V_Int (P.Vector i _ arr)) j =
        indexByteArrayAsMulti arr (i+j)

instance PackedMVector U.MVector Int where
    unsafeReadAsMulti (MV_Int (P.MVector i _ arr)) j =
        readByteArrayAsMulti arr (i+j)

    unsafeWriteAsMulti (MV_Int (P.MVector i _ arr)) j x =
        writeByteArrayAsMulti arr (i+j) x
#endif
