{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
-- {-# OPTIONS_GHC -W -Werror #-}

module Main where

import Prelude hiding (map, zipWith, foldl)

import Data.Primitive.Multi
import Data.Primitive.Multi.FloatX4
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Packed
import qualified Data.Vector.Generic as G1
import qualified Data.Vector.Generic.New as GN
import qualified Data.Vector.Fusion.Stream.Monadic as S
import qualified Data.Vector.Generic.MultiStream as G
import qualified Data.Vector.Fusion.MultiStream.Monadic as MS

import GHC.Float
import GHC.Prim
import GHC.Types

import Util

{-# RULES

"multistream/unstream" forall (x :: forall v . G1.Vector v Float => v Float) .
  G.multistream (G1.new (GN.unstream x)) = adaptMultiFloat x

  #-}

main :: IO ()
main = do
    timeComp (\_ -> return $ sumFrom1To mx)
       (\x -> "   Multi lazy: " ++ show x)
  where
    mx :: Float
    mx = 10000000

sumFrom1To :: Float -> Float
sumFrom1To mx = G.foldl' (+) (+) red 0 v
  where
    v :: U.Vector Float
    v = U.enumFromTo 1 mx

    red :: Multi Float -> Float
    red (MultiFloat (FX4# fv)) =
        let !(# a, b, c, d #) = unpackFloatX4# fv
        in
          F# (a `plusFloat#` b `plusFloat#` c `plusFloat#` d)

adaptMultiFloat :: forall m . Monad m
                => S.Stream m Float
                -> MS.Stream m Float
adaptMultiFloat (S.Stream step (s :: s) sz) =
    MS.Stream step' (s, 0, 0) id step1' sz
  where
    step' :: (s, Int, Multi Float)
          -> m (MS.Step (s, Int, Multi Float) (Multi Float))
    step' (s, i@(I# i#), v@(MultiFloat (FX4# v#))) = do
        r <- step s
        case r of
          S.Yield !(F# f#) s' ->
              let !v'# = insertFloatX4# v# f# i#
                  v' = MultiFloat (FX4# v'#)
              in
                return $ if i == 3
                         then MS.Yield v' (s', 0, v')
                         else MS.Skip     (s', i + 1, v')
          S.Skip s' -> return $ MS.Skip (s', i, v)
          S.Done    -> return $ MS.Done

    step1' :: (s, Int, Multi Float)
           -> m (MS.Step (s, Int, Multi Float) Float)
    step1' (s, 3, v@(MultiFloat (FX4# v#))) =
        let! (# _, _, x, _ #) = unpackFloatX4# v#
        in
          return $ MS.Yield (F# x) (s, 2, v)

    step1' (s, 2, v@(MultiFloat (FX4# v#))) =
        let! (# _, x, _, _ #) = unpackFloatX4# v#
        in
          return $ MS.Yield (F# x) (s, 1, v)

    step1' (s, 1, v@(MultiFloat (FX4# v#))) =
        let! (# x, _, _, _ #) = unpackFloatX4# v#
        in
          return $ MS.Yield (F# x) (s, 0, v)

    step1' (s, 0, v) = do
        r <- step s
        case r of
          S.Yield x s' -> return $ MS.Yield x (s', 0, v)
          S.Skip    s' -> return $ MS.Skip    (s', 0, v)
          S.Done       -> return $ MS.Done
