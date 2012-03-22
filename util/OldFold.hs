{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OldFold (
    u_mfoldl'
  ) where

import "vector" Data.Primitive.Multi
import Data.Vector.Fusion.Stream.Monadic (MultiUnf(..), Step(..), SPEC(..))
import qualified Data.Vector.Fusion.Stream as S
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Generic as G
import Data.Vector.Fusion.Util (unId)
import qualified Data.Vector.Unboxed as U

mstream_mfoldlM'  ::  Monad m
                  =>  (a -> Multi c -> m a)
                  ->  (b -> c -> m b)
                  ->  a
                  ->  (a -> b)
                  ->  SM.Stream m v c
                  ->  m b
{-# INLINE [1] mstream_mfoldlM' #-}
mstream_mfoldlM' p q z m21 (SM.Stream _ (Left (MultiUnf stepm step1 s)) _ _) =
    mfoldlM_loopm SPEC z s
  where
    mfoldlM_loopm !sPEC z s
      = z `seq` do
          r <- stepm s
          case r of
            Yield x s' -> do { z' <- p z x; mfoldlM_loopm SPEC z' s' }
            Skip    s' -> mfoldlM_loopm SPEC z s'
            Done       -> mfoldlM_loop1 SPEC (m21 z) s

    mfoldlM_loop1 !sPEC z s
      = z `seq` do
          r <- step1 s
          case r of
            Yield x s' -> do { z' <- q z x; mfoldlM_loop1 SPEC z' s' }
            Skip    s' -> mfoldlM_loop1 SPEC z s'
            Done       -> return z

mstream_mfoldlM' p q z m21 s =
    SM.foldM' q (m21 z) s

mstream_mfoldl'  ::  Monad m
                 =>  (a -> Multi c -> a)
                 ->  (b -> c -> b)
                 ->  a
                 ->  (a -> b)
                 ->  SM.Stream m v c
                 ->  m b
{-# INLINE mstream_mfoldl' #-}
mstream_mfoldl' p q z m21 =
    mstream_mfoldlM' (\a b -> return (p a b)) (\a b -> return (q a b)) z m21

stream_mfoldl' ::  (a -> Multi c -> a)
               ->  (b -> c -> b)
               ->  a
               ->  (a -> b)
               ->  S.Stream v c
               ->  b
{-# INLINE stream_mfoldl' #-}
stream_mfoldl' p q z m21 = unId . mstream_mfoldl' p q z m21

g_mfoldl' :: G.PackedVector v c
          => (a -> Multi c -> a)
          -> (b -> c -> b)
          -> a
          -> (a -> b)
          -> v c
          -> b
{-# INLINE g_mfoldl' #-}
g_mfoldl' p q z m21 = stream_mfoldl' p q z m21 . G.multistream

u_mfoldl' :: G.PackedVector v c
          => (a -> Multi c -> a)
          -> (b -> c -> b)
          -> a
          -> (a -> b)
          -> v c
          -> b
{-# INLINE u_mfoldl' #-}
u_mfoldl'= g_mfoldl'
