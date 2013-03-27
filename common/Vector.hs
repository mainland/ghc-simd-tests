{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if !defined(STORABLE)
#define STORABLE 1
#endif /* !defined(STORABLE) */

module Vector (
#if STORABLE
    module Data.Vector.Storable,
#else /* !STORABLE */
    module Data.Vector.Unboxed,
#endif /* !STORABLE */
    randomVector,
    toPArray,
    unsafeToPtrLen,

    vmap,
    vfold,
    vsum,
    vproduct,
    vzipWith
 ) where

import Data.Array.Parallel (PArray)
import Data.Primitive.Multi
import qualified Data.Vector.Generic as G
import Foreign.C.Types
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr
import System.Random (Random)

#if STORABLE
import Data.Array.Parallel.PArray (PA, nf, fromVector)
import Data.Vector.Storable
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
#else /* !STORABLE */
import Data.Array.Parallel.PArray (nf, fromUArray)
import Data.Array.Parallel.PArray.Scalar (Scalar)
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Primitive (sizeOf)
import qualified Data.Vector.Primitive as P
import Data.Vector.Unboxed
import qualified Data.Vector.Unboxed as U
import GHC.Ptr
#endif /* !STORABLE */

import Util.Random

#if STORABLE
randomVector :: (Storable a,Random a) => Int -> (a, a) -> IO (Vector a)
randomVector = randomMM

toPArray :: (PA a, Storable a) => Vector a -> PArray a
toPArray = fromVector . U.convert

unsafeToPtrLen :: Storable a => Vector a -> (Ptr a, CInt)
{-# INLINE unsafeToPtrLen #-}
unsafeToPtrLen v =
    (unsafeForeignPtrToPtr vfp, fromIntegral n)
  where
    (vfp, n) = unsafeToForeignPtr0 v

vmap  ::  (G.PackedVector Vector a, Num a, Num (Multi a))
      =>  (forall a . Num a => a -> a)
      ->  Vector a -> Vector a
{-# INLINE vmap #-}
vmap f u = S.mmap f f u

vfold  ::  (G.PackedVector Vector a, Num a, Num (Multi a))
       =>  (forall a . Num a => a -> a -> a)
       ->  a
       ->  Vector a
       ->  a
{-# INLINE vfold #-}
vfold f z u = S.mfold' f f z u

vsum  ::  (G.PackedVector Vector a, Num a, Num (Multi a))
      =>  Vector a
      ->  a
{-# INLINE vsum #-}
vsum u = vfold (+) 0 u

vproduct  ::  (G.PackedVector Vector a, Num a, Num (Multi a))
          =>  Vector a
          ->  a
{-# INLINE vproduct #-}
vproduct u = vfold (*) 1 u

vzipWith  ::  (G.PackedVector Vector a, Num a, Num (Multi a))
          =>  (forall a . Num a => a -> a -> a)
          ->  Vector a -> Vector a -> Vector a
{-# INLINE vzipWith #-}
vzipWith f u v = S.mzipWith f f u v
#else /* !STORABLE */
randomVector :: (Unbox a,Random a) => Int -> (a, a) -> IO (Vector a)
randomVector = randomU

toPArray :: Scalar a => Vector a -> PArray a
toPArray = fromUArray

class UnsafeToPtrLen a where
    unsafeToPtrLen :: Vector a -> (Ptr a, CInt)

instance UnsafeToPtrLen Float where
    {-# INLINE unsafeToPtrLen #-}
    unsafeToPtrLen (V_Float (P.Vector off len arr)) =
        (p, fromIntegral (fromIntegral (len - off)))
      where
        p :: Ptr Float
        p = case byteArrayContents arr `plusAddr` off*sz of
              Addr a -> Ptr a

        sz :: Int
        sz = sizeOf (undefined :: Float)

instance UnsafeToPtrLen Double where
    {-# INLINE unsafeToPtrLen #-}
    unsafeToPtrLen (V_Double (P.Vector off len arr)) =
        (p, fromIntegral (fromIntegral (len - off)))
      where
        p :: Ptr Double
        p = case byteArrayContents arr `plusAddr` off*sz of
              Addr a -> Ptr a

        sz :: Int
        sz = sizeOf (undefined :: Double)

vmap  ::  (G.PackedVector Vector a, Unbox a, Num a, Num (Multi a))
      =>  (forall a . Num a => a -> a)
      ->  Vector a -> Vector a
{-# INLINE vmap #-}
vmap f u = U.mmap f f u

vfold  ::  (G.PackedVector Vector a, Unbox a, Num a, Num (Multi a))
       =>  (forall a . Num a => a -> a -> a)
       ->  a
       ->  Vector a
       ->  a
{-# INLINE vfold #-}
vfold f z u = U.mfold' f f z u

vsum  ::  (G.PackedVector Vector a, Unbox a, Num a, Num (Multi a))
      =>  Vector a
      ->  a
{-# INLINE vsum #-}
vsum u = vfold (+) 0 u

vproduct  ::  (G.PackedVector Vector a, Unbox a, Num a, Num (Multi a))
          =>  Vector a
          ->  a
{-# INLINE vproduct #-}
vproduct u = vfold (*) 1 u

vzipWith  ::  (G.PackedVector Vector a, Unbox a, Num a, Num (Multi a))
          =>  (forall a . Num a => a -> a -> a)
          ->  Vector a -> Vector a -> Vector a
{-# INLINE vzipWith #-}
vzipWith f u v = U.mzipWith f f u v
#endif /* !STORABLE */
