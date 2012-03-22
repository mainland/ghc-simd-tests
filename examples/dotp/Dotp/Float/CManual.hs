{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dotp.Float.CManual (
    dotp
  ) where

import qualified Data.Vector.Unboxed as U

import Foreign.C
import Foreign.Ptr

import Util

foreign import ccall "cvecdotp" c_vecdotp :: Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat

dotp :: U.Vector Float -> U.Vector Float -> Float
{-# INLINE dotp #-}
dotp u v =
    (fromRational . toRational) (c_vecdotp up ul vp vl)
  where
    up, vp :: Ptr CFloat
    ul, vl :: CInt
    (up, ul) = unsafeFloatUVectorToPtr u
    (vp, vl) = unsafeFloatUVectorToPtr v
