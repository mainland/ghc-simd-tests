{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -W #-}

module Data.Vector.Fusion.MultiStream.Monadic (
    Step(..), Stream(..),
    map, mapM, mapM_,
    zipWithM, zipWith,
    foldl, foldlM, foldl', foldlM'
  )  where

import Data.Primitive.Multi
import Data.Vector.Fusion.Stream.Monadic (Step(..))
import Data.Vector.Fusion.Stream.Size (Size(..), smaller)

import Prelude hiding ( length, null,
                        replicate, (++),
                        head, last, (!!),
                        init, tail, take, drop,
                        map, mapM, mapM_, concatMap,
                        zipWith, zipWith3, zip, zip3,
                        filter, takeWhile, dropWhile,
                        elem, notElem,
                        foldl, foldl1, foldr, foldr1,
                        and, or,
                        scanl, scanl1,
                        enumFromTo, enumFromThenTo )

#if __GLASGOW_HASKELL__ >= 700
import GHC.Exts ( SpecConstrAnnotation(..) )
#endif

#include "vector.h"

data SPEC = SPEC | SPEC2
#if __GLASGOW_HASKELL__ >= 700
{-# ANN type SPEC ForceSpecConstr #-}
#endif

data Stream m a = forall s s1. Stream (s -> m (Step s (Multi a)))
                                      s
                                      (s -> s1)
                                      (s1 -> m (Step s1 a))
                                      Size

--
-- maps
--

map :: Monad m
    => (a -> b)
    -> (Multi a -> Multi b)
    -> Stream m a
    -> Stream m b
{-# INLINE map #-}
map f1 f = mapM (return . f1) (return . f)

mapM :: forall a b m . Monad m
     => (a -> m b)
     -> (Multi a -> m (Multi b))
     -> Stream m a
     -> Stream m b
{-# INLINE_STREAM mapM #-}
mapM f1 f (Stream step (s :: s) (s2s1 :: s -> s1) step1 sz) =
    Stream step' s s2s1 step1' sz
  where
    step' :: s -> m (Step s (Multi b))
    {-# INLINE_INNER step' #-}
    step' s = do
        r <- step s
        case r of
          Yield  x s' -> f x >>= \z ->
                         return $ Yield z s'
          Skip     s' -> return $ Skip    s'
          Done        -> return $ Done

    step1' :: s1 -> m (Step s1 b)
    {-# INLINE_INNER step1' #-}
    step1' s1 = do
        r1 <- step1 s1
        case r1 of
          Yield  x s1' -> f1 x >>= \z ->
                          return $ Yield z s1'
          Skip     s1' -> return $ Skip    s1'
          Done         -> return $ Done

consume :: Monad m => Stream m a -> m ()
{-# INLINE_STREAM consume #-}
consume (Stream step s s2s1 step1 _) = consume_loop SPEC s
  where
    consume_loop !_ s = do
        r <- step s
        case r of
          Yield  _ s' -> consume_loop  SPEC s'
          Skip     s' -> consume_loop  SPEC s'
          Done        -> consume_loop1 SPEC (s2s1 s)

    consume_loop1 !_ s1 = do
        r1 <- step1 s1
        case r1 of
          Yield  _ s1' -> consume_loop1 SPEC s1'
          Skip     s1' -> consume_loop1 SPEC s1'
          Done         -> return ()

mapM_ :: Monad m
      => (a -> m b)
      -> (Multi a -> m (Multi b))
      -> Stream m a
      -> m ()
{-# INLINE_STREAM mapM_ #-}
mapM_ f1 f = consume . mapM f1 f

--
-- zips
--

{-# RULES

"zipWithM xs xs [PackedVector.Stream]" forall f1 f xs.
  zipWithM f1 f xs xs = mapM (\x -> f1 x x) (\x -> f x x) xs

  #-}

zipWithM :: forall a b c m . Monad m
         => (a -> b -> m c)
         -> (Multi a -> Multi b -> m (Multi c))
         -> Stream m a
         -> Stream m b
         -> Stream m c
{-# INLINE_STREAM zipWithM #-}
zipWithM f1 f (Stream stepa sa sa2s1a step1a sza)
              (Stream stepb sb sb2s1b step1b szb) =
    Stream step (sa, sb, Nothing) s2s1 step1 (smaller sza szb)
  where
    {-# INLINE_INNER step #-}
    step (sa, sb, Nothing) = do
        r <- stepa sa
        case r of
          Yield x sa' -> return $ Skip (sa', sb, Just x)
          Skip    sa' -> return $ Skip (sa', sb, Nothing)
          Done        -> return $ Done

    step (sa, sb, Just x) = do
        r <- stepb sb
        case r of
          Yield y sb' -> f x y >>= \z ->
                         return $ Yield z (sa, sb', Nothing)
          Skip    sb' -> return $ Skip    (sa, sb', Just x)
          Done        -> return $ Done

    {-# INLINE_INNER step1 #-}
    step1 (s1a, s1b, Nothing) = do
        r <- step1a s1a
        case r of
          Yield x s1a' -> return $ Skip (s1a', s1b, Just x)
          Skip    s1a' -> return $ Skip (s1a', s1b, Nothing)
          Done         -> return $ Done

    step1 (s1a, s1b, Just x) = do
        r <- step1b s1b
        case r of
          Yield y s1b' -> f1 x y >>= \z ->
                          return $ Yield z (s1a, s1b', Nothing)
          Skip    s1b' -> return $ Skip    (s1a, s1b', Just x)
          Done         -> return $ Done

    {-# INLINE_INNER s2s1 #-}
    s2s1 (sa, sb, _) = (sa2s1a sa, sb2s1b sb, Nothing)

zipWith :: Monad m
        => (a -> b -> c)
        -> (Multi a -> Multi b -> Multi c)
        -> Stream m a
        -> Stream m b
        -> Stream m c
{-# INLINE zipWith #-}
zipWith f1 f = zipWithM (\a b -> return (f1 a b)) (\a b -> return (f a b))

--
-- folds
--

-- | Left fold
foldl :: Monad m
      => (a -> b -> a)
      -> (Multi a -> Multi b -> Multi a)
      -> (Multi a -> a)
      -> Multi a
      -> Stream m b
      -> m a
{-# INLINE foldl #-}
foldl f1 f =
    foldlM (\a b -> return (f1 a b)) (\a b -> return (f a b))

-- | Left fold with a monadic operator
foldlM :: Monad m
        => (a -> b -> m a)
        -> (Multi a -> Multi b -> m (Multi a))
        -> (Multi a -> a)
        -> Multi a
        -> Stream m b
        -> m a
{-# INLINE_STREAM foldlM #-}
foldlM f1 f red z (Stream step s s2s1 step1 _) =
    foldlM_loop SPEC z s
  where
    {-# INLINE_INNER foldlM_loop #-}
    foldlM_loop !sPEC z s = do
        r <- step s
        case r of
          Yield x s' -> f z x >>= \z' ->
                        foldlM_loop SPEC z' s'
          Skip    s' -> foldlM_loop SPEC z  s'
          Done       -> foldlM_loop1 sPEC (red z) (s2s1 s)

    {-# INLINE_INNER foldlM_loop1 #-}
    foldlM_loop1 !sPEC z s = do
        r <- step1 s
        case r of
          Yield x s' -> f1 z x >>= \z' ->
                        foldlM_loop1 SPEC z' s'
          Skip    s' -> foldlM_loop1 SPEC z  s'
          Done       -> return z

-- | Left fold with a strict accumulator
foldl' :: Monad m
       => (a -> b -> a)
       -> (Multi a -> Multi b -> Multi a)
       -> (Multi a -> a)
       -> Multi a
       -> Stream m b
       -> m a
{-# INLINE foldl' #-}
foldl' f1 f =
    foldlM' (\a b -> return (f1 a b)) (\a b -> return (f a b))

-- | Left fold with a strict accumulator and a monadic operator
foldlM' :: Monad m
         => (a -> b -> m a)
         -> (Multi a -> Multi b -> m (Multi a))
         -> (Multi a -> a)
         -> Multi a
         -> Stream m b
         -> m a
{-# INLINE_STREAM foldlM' #-}
foldlM' f1 f red z (Stream step s s2s1 step1 _) =
    foldlM'_loop SPEC z s
  where
    {-# INLINE_INNER foldlM'_loop #-}
    foldlM'_loop !sPEC z s = z `seq` do
        r <- step s
        case r of
          Yield x s' -> f z x >>= \z' ->
                        foldlM'_loop  SPEC z'      s'
          Skip    s' -> foldlM'_loop  SPEC z       s'
          Done       -> foldlM'_loop1 SPEC (red z) (s2s1 s)

    {-# INLINE_INNER foldlM'_loop1 #-}
    foldlM'_loop1 !sPEC z s = z `seq` do
        r <- step1 s
        case r of
          Yield x s' -> f1 z x >>= \z' ->
                        foldlM'_loop1 SPEC z' s'
          Skip    s' -> foldlM'_loop1 SPEC z  s'
          Done       -> return z
