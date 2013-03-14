{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dotp.Float.CManual (
    dotp
  ) where

import Foreign.C
import Foreign.Ptr

import qualified Vector as V

foreign import ccall unsafe "cvecdotp" c_vecdotp :: Ptr Float -> CInt -> Ptr Float -> CInt -> CFloat

dotp :: V.Vector Float -> V.Vector Float -> Float
{-# INLINE dotp #-}
dotp u v =
    realToFrac (c_vecdotp up ul vp vl)
  where
    up, vp :: Ptr Float
    ul, vl :: CInt
    (up, ul) = V.unsafeToPtrLen u
    (vp, vl) = V.unsafeToPtrLen v
