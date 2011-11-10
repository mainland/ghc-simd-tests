{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -W -Werror #-}

module Data.Vector.Unboxed.Packed where

import Control.Monad.Primitive
import Data.Vector.Generic as V
import Data.Vector.Generic.Mutable as M

import Data.Primitive.Multi

class (MultiPrim a, V.Vector v a, PackedMVector (Mutable v) a) => PackedVector v a where
    unsafeIndexMulti :: v a -> Int -> Multi a

class (MultiPrim a, M.MVector v a) => PackedMVector v a where
    unsafeReadMulti  :: (PrimMonad m) => v (PrimState m) a -> Int -> m (Multi a)
    unsafeWriteMulti :: (PrimMonad m) => v (PrimState m) a -> Int -> Multi a -> m ()
