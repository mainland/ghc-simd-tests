{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dotp.Double.CManual (
    dotp
  ) where

import qualified Data.Vector.Unboxed as U

import Foreign.C
import Foreign.Ptr

import Util.Unsafe

foreign import ccall "cdvecdotp" c_dvecdotp :: Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble

dotp :: U.Vector Double -> U.Vector Double -> Double
{-# INLINE dotp #-}
dotp u v =
    (fromRational . toRational) (c_dvecdotp up ul vp vl)
  where
    up, vp :: Ptr CDouble
    ul, vl :: CInt
    (up, ul) = unsafeDoubleUVectorToPtr u
    (vp, vl) = unsafeDoubleUVectorToPtr v
