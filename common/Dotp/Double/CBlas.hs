{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dotp.Double.CBlas (
    dotp
  ) where

import qualified Data.Vector.Unboxed as U

import Foreign.C
import Foreign.Ptr

import Util.Unsafe

foreign import ccall "cblas_ddot" cblas_ddot :: CInt -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble

dotp :: U.Vector Double -> U.Vector Double -> Double
{-# INLINE dotp #-}
dotp u v =
    (fromRational . toRational) (cblas_ddot (min ul vl) up 1 vp 1)
  where
    up, vp :: Ptr CDouble
    ul, vl :: CInt
    (up, ul) = unsafeDoubleUVectorToPtr u
    (vp, vl) = unsafeDoubleUVectorToPtr v
