{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Rbf.Double.CManual (
    rbf
  ) where

import Foreign.C
import Foreign.Ptr

import qualified Vector as V

foreign import ccall "crbf" c_rbf :: CDouble -> Ptr Double -> CInt -> Ptr Double -> CInt -> CDouble

rbf :: Double -> V.Vector Double -> V.Vector Double -> Double
{-# INLINE rbf #-}
rbf nu u v =
    (fromRational . toRational) (c_rbf ((fromRational . toRational) nu) up ul vp vl)
  where
    up, vp :: Ptr Double
    ul, vl :: CInt
    (up, ul) = V.unsafeToPtrLen u
    (vp, vl) = V.unsafeToPtrLen v
