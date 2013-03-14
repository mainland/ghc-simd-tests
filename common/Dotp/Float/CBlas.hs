{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dotp.Float.CBlas (
    dotp
  ) where

import Foreign.C
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr

import qualified Vector as V

foreign import ccall unsafe "cblas_sdot" cblas_sdot :: CInt -> Ptr Float -> CInt -> Ptr Float -> CInt -> CFloat

dotp :: V.Vector Float -> V.Vector Float -> Float
{-# INLINE dotp #-}
dotp u v =
    realToFrac (cblas_sdot (min ul vl) up 1 vp 1)
  where
    up, vp :: Ptr Float
    ul, vl :: CInt
    (up, ul) = V.unsafeToPtrLen u
    (vp, vl) = V.unsafeToPtrLen v
