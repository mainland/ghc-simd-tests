{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dotp.Float.CScalar (
    dotp
  ) where

import qualified Data.Vector.Unboxed as U

import Foreign.C
import Foreign.Ptr

import Util.Unsafe

foreign import ccall "cdotp" c_dotp :: Ptr CFloat -> CInt -> Ptr CFloat -> CInt -> CFloat

dotp :: U.Vector Float -> U.Vector Float -> Float
{-# INLINE dotp #-}
dotp u v =
    (fromRational . toRational) (c_dotp up ul vp vl)
  where
    up, vp :: Ptr CFloat
    ul, vl :: CInt
    (up, ul) = unsafeFloatUVectorToPtr u
    (vp, vl) = unsafeFloatUVectorToPtr v
