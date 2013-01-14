{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dotp.Double.CScalar (
    dotp
  ) where

import qualified Data.Vector.Unboxed as U

import Foreign.C
import Foreign.Ptr

import Util.Unsafe

foreign import ccall "cddotp" c_dotp :: Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble

dotp :: U.Vector Double -> U.Vector Double -> Double
{-# INLINE dotp #-}
dotp u v =
    (fromRational . toRational) (c_dotp up ul vp vl)
  where
    up, vp :: Ptr CDouble
    ul, vl :: CInt
    (up, ul) = unsafeDoubleUVectorToPtr u
    (vp, vl) = unsafeDoubleUVectorToPtr v
