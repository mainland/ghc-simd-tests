{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Dotp.Double.CManual (
    dotp
  ) where

import Foreign.C
import Foreign.Ptr

import qualified Vector as V

foreign import ccall "cdvecdotp" c_dvecdotp :: Ptr Double -> CInt -> Ptr Double -> CInt -> CDouble

dotp :: V.Vector Double -> V.Vector Double -> Double
{-# INLINE dotp #-}
dotp u v =
    (fromRational . toRational) (c_dvecdotp up ul vp vl)
  where
    up, vp :: Ptr Double
    ul, vl :: CInt
    (up, ul) = V.unsafeToPtrLen u
    (vp, vl) = V.unsafeToPtrLen v
