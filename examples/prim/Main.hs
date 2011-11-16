{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import GHC.Prim
import GHC.Types

import Data.Primitive.Multi.FloatX4

main :: IO ()
main = do
    print $ FX4# v#
    print $ FX4# (foo v#)
    print $ FX4# (bar 0.0 v# 0.0)
    let g = bar 0.0
    print $ FX4# (g v# 0.0)
    print $ FX4# (app (\x -> x) v#)
  where
    !v# = packFloatX4# 1.0# 2.0# 3.0# 4.0#

foo :: FloatX4# -> FloatX4#
{-# NOINLINE foo #-}
foo x = x

bar :: Float -> FloatX4# -> Double -> FloatX4#
{-# NOINLINE bar #-}
bar _ x _ =
    case x of
      _ -> x

app :: (FloatX4# -> FloatX4#) -> FloatX4# -> FloatX4#
{-# NOINLINE app #-}
app f x = f x