{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dotp.Double.CScalar (
    dotp
  ) where

import Foreign.C
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr

import qualified Vector as V

foreign import ccall "cddotp" c_dotp :: Ptr Double -> CInt -> Ptr Double -> CInt -> CDouble

dotp :: V.Vector Double -> V.Vector Double -> Double
{-# INLINE dotp #-}
dotp u v =
    (fromRational . toRational) (c_dotp up ul vp vl)
  where
    up, vp :: Ptr Double
    ul, vl :: CInt
    (up, ul) = V.unsafeToPtrLen u
    (vp, vl) = V.unsafeToPtrLen v
