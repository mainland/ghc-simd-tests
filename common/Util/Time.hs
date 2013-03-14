{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.Time (
    getCPUTime,
    getWallTime
  ) where

foreign import ccall unsafe "getCPUTime"  getCPUTime  :: IO Double
foreign import ccall unsafe "getWallTime" getWallTime :: IO Double
