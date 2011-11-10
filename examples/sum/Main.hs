{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -W -Werror #-}

module Main where

import qualified Data.Vector.Unboxed as U

import Util

import qualified MultiLazy
import qualified MultiStrict
import qualified SingleLazy
import qualified SingleStrict

{- Sum vectors of Floats. The lazy versions do not manifest a vector. The
 - Multi - versions use Multi Float's for computation.
 -}

main :: IO ()
main = do
    timeComp (\_ -> return $ SingleLazy.sumFrom1To mx)
       (\x -> "  Single lazy: " ++ show x)

    timeComp (\_ -> return $ MultiLazy.sumFrom1To mx)
       (\x -> "   Multi lazy: " ++ show x)

    timeComp (\_ -> return $ SingleStrict.sumFrom1To v)
       (\x -> "Single strict: " ++ show x)

    timeComp (\_ -> return $ MultiStrict.sumFrom1To v)
       (\x -> " Multi strict: " ++ show x)
  where
    mx :: Float
    mx = 10000000

    v :: U.Vector Float
    !v = U.enumFromTo 1 mx
