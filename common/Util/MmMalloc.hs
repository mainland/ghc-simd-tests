{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.MmMalloc (
    mmMallocForeignPtr
  ) where

import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

foreign import ccall unsafe "mm_malloc"
  mmMalloc_ :: CSize -> CSize -> IO (Ptr a)

foreign import ccall unsafe "&mm_free"
  mmFree_ :: FunPtr (Ptr a -> IO ())

mmMallocForeignPtr :: forall a . Storable a => Int -> Int -> IO (ForeignPtr a)
mmMallocForeignPtr size align =
    mmMalloc_ (fromIntegral (size*sizeOf (undefined :: a)))
                  (fromIntegral align) >>=
    newForeignPtr mmFree_
