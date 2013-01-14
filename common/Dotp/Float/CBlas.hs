{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dotp.Float.CBlas (
    dotp
  ) where

import qualified Data.Vector.Unboxed as U

import Foreign.C
import Foreign.Ptr

import Util.Unsafe

foreign import ccall "cblas_sdot" cblas_sdot :: CInt -> Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat

dotp :: U.Vector Float -> U.Vector Float -> Float
{-# INLINE dotp #-}
dotp u v =
    (fromRational . toRational) (cblas_sdot (min ul vl) up 1 vp 1)
  where
    up, vp :: Ptr CFloat
    ul, vl :: CInt
    (up, ul) = unsafeFloatUVectorToPtr u
    (vp, vl) = unsafeFloatUVectorToPtr v
