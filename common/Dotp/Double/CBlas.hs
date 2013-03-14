{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dotp.Double.CBlas (
    dotp
  ) where

import Foreign.C
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr

import qualified Vector as V

foreign import ccall unsafe "cblas_ddot" cblas_ddot :: CInt -> Ptr Double -> CInt -> Ptr Double -> CInt -> CDouble

dotp :: V.Vector Double -> V.Vector Double -> Double
{-# INLINE dotp #-}
dotp u v =
    realToFrac (cblas_ddot (min ul vl) up 1 vp 1)
  where
    up, vp :: Ptr Double
    ul, vl :: CInt
    (up, ul) = V.unsafeToPtrLen u
    (vp, vl) = V.unsafeToPtrLen v
