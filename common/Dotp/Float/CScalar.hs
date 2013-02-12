{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dotp.Float.CScalar (
    dotp
  ) where

import Foreign.C
import Foreign.Ptr

import qualified Vector as V

foreign import ccall "cdotp" c_dotp :: Ptr Float -> CInt -> Ptr Float -> CInt -> CFloat

dotp :: V.Vector Float -> V.Vector Float -> Float
{-# INLINE dotp #-}
dotp u v =
    (fromRational . toRational) (c_dotp up ul vp vl)
  where
    up, vp :: Ptr Float
    ul, vl :: CInt
    (up, ul) = V.unsafeToPtrLen u
    (vp, vl) = V.unsafeToPtrLen v
