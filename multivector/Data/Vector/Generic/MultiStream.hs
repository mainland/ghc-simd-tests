{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -W #-}

module Data.Vector.Generic.MultiStream where

import Prelude hiding (map, zipWith, foldl)

import Control.Monad.Primitive
import Data.Primitive.Multi
import Data.Vector.Fusion.Stream.Size
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Vector.Generic.New (New(..))
import Data.Vector.Unboxed.Packed
import qualified Data.Vector.Fusion.MultiStream as F
import Data.Vector.Fusion.MultiStream.Monadic as M

#include "vector.h"

--
-- stream and unstream
--

mmultistream :: forall m a v . (PrimMonad m, PackedMVector v a)
             => v (PrimState m) a
             -> Stream m a
{-# INLINE_STREAM mmultistream #-}
mmultistream v =
    v `seq` n `seq` k `seq` m `seq` Stream step 0 id step1 (Max n)
  where
    n,k,m :: Int
    n = GM.length v
    k = n - n `rem` m
    m = multiplicity (undefined :: Multi a)

    {-# INLINE_INNER step #-}
    step :: Int -> m (Step Int (Multi a))
    step i | i >= k =
        return Done

    step i = do
      x <- unsafeReadAsMulti v i
      return (Yield x (i + m))

    {-# INLINE_INNER step1 #-}
    step1 :: Int -> m (Step Int a)
    step1 i | i >= n =
        return Done

    step1 i = do
      x <- GM.unsafeRead v i
      return (Yield x (i + 1))

munmultistream :: (PrimMonad m, PackedMVector v a)
               => Stream m a
               -> m (v (PrimState m) a)
{-# INLINE_STREAM munmultistream #-}
munmultistream s@(Stream _ _ _ _ sz) =
    case upperBound sz of
      Just n  -> unstreamMax     s n
      Nothing -> unstreamUnknown s

unstreamMax :: forall m v a . (PrimMonad m, PackedMVector v a)
            => Stream m a
            -> Int
            -> m (v (PrimState m) a)
{-# INLINE unstreamMax #-}
unstreamMax (Stream step s s2s1 step1 _) n = do
    v  <- GM.unsafeNew n
    n' <- loop v 0 s
    return $ GM.unsafeSlice 0 n' v
  where
    m :: Int
    m = multiplicity (undefined :: Multi a)

    {-# INLINE_INNER loop #-}
    loop v i s = do
        r <- step s
        case r of
          Yield x s' -> unsafeWriteAsMulti v i x >> loop v (i+m) s'
          Skip    s' -> loop  v i s'
          Done       -> loop1 v i (s2s1 s)

    {-# INLINE_INNER loop1 #-}
    loop1 v i s = do
        r <- step1 s
        case r of
          Yield x s' -> GM.unsafeWrite v i x >> loop1 v (i+1) s'
          Skip    s' -> loop1 v i s'
          Done       -> return i

unstreamUnknown :: Stream m a -> m (v (PrimState m) a)
{-# INLINE unstreamUnknown #-}
unstreamUnknown _ = error ":/"

-- The seq on n is extremely important because it ensures that tests on n occur
-- earlier. This allows the simplifier to use the results of these tests sooner,
-- giving us better loops.
multistream :: forall a v . (PackedVector v a)
            => v a
            -> F.Stream a
{-# INLINE_STREAM multistream #-}
multistream v =
    v `seq` n `seq` k `seq` m `seq` Stream step 0 id step1 (Exact n)
  where
    n,k,m :: Int
    n = G.length v
    k = n - n `rem` m
    m = multiplicity (undefined :: Multi a)

    {-# INLINE_INNER step #-}
    -- step :: Int -> m (Step Int (Multi a))
    step i | i >= k =
        return Done

    step i = do
      let x = unsafeIndexAsMulti v i
      return $ Yield x (i + m)

    {-# INLINE_INNER step1 #-}
    -- step1 :: Int -> m (Step Int a)
    step1 i | i >= n =
        return Done

    step1 i = do
      let x = G.unsafeIndex v i
      return $ Yield x (i + 1)

{-# RULES

"multistream/unmultistream'" forall v .
  multistream (G.new (unmultistream' v)) = v

  #-}

unmultistream' :: (PackedVector v a)
              => F.Stream a
              -> New v a
{-# INLINE unmultistream #-}
unmultistream' s =
    New (munmultistream (F.liftStream s))

unmultistream :: PackedVector v a => F.Stream a -> v a
{-# INLINE_STREAM unmultistream' #-}
unmultistream s = G.new (unmultistream' s)

map :: (PackedVector v a, PackedVector v b)
    => (a -> b)
    -> (Multi a -> Multi b)
    -> v a
    -> v b
{-# INLINE map #-}
map f1 f = unmultistream . F.inplace (M.map f1 f) . multistream

zipWith :: (PackedVector v a, PackedVector v b, PackedVector v c)
        => (a -> b -> c)
        -> (Multi a -> Multi b -> Multi c)
        -> v a
        -> v b
        -> v c
{-# INLINE zipWith #-}
zipWith f1 f v1 v2 = unmultistream $ F.zipWith f1 f (multistream v1) (multistream v2)

foldl :: (PackedVector v b)
      => (a -> b -> a)
      -> (Multi a -> Multi b -> Multi a)
      -> (Multi a -> a)
      -> Multi a
      -> v b
      -> a
{-# INLINE foldl #-}
foldl f1 f z red = F.foldl f1 f z red . multistream

foldl' :: (PackedVector v b)
       => (a -> b -> a)
       -> (Multi a -> Multi b -> Multi a)
       -> (Multi a -> a)
       -> Multi a
       -> v b
       -> a
{-# INLINE foldl' #-}
foldl' f1 f red z = F.foldl' f1 f red z . multistream
