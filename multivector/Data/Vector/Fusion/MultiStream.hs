{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Data.Vector.Fusion.MultiStream (
  -- * Types
  Step(..), Stream, MStream,
  inplace, liftStream, map, zipWith, foldl, foldl'
) where

import Prelude hiding (map, zipWith, foldl)

import Data.Vector.Fusion.MultiStream.Monadic ( Step(..) )
import qualified Data.Vector.Fusion.MultiStream.Monadic as M
import Data.Vector.Fusion.Util

import Data.Primitive.Multi

#include "vector.h"

-- | The type of pure streams
type Stream = M.Stream Id

-- | Alternative name for monadic streams
type MStream = M.Stream

inplace :: (forall m. Monad m => M.Stream m a -> M.Stream m b)
        -> Stream a -> Stream b
{-# INLINE_STREAM inplace #-}
inplace f s = s `seq` f s

-- | Convert a pure stream to a monadic stream
liftStream :: Monad m => Stream a -> MStream m a
{-# INLINE_STREAM liftStream #-}
liftStream (M.Stream step s s2s1 step1 sz) =
    M.Stream (return . unId . step) s s2s1 (return . unId . step1) sz

{-# RULES

"inplace/inplace [Vector]"
  forall (f :: forall m. Monad m => MStream m a -> MStream m a)
         (g :: forall m. Monad m => MStream m a -> MStream m a)
         s.
  inplace f (inplace g s) = inplace (f . g) s

  #-}

-- | Map a function over a 'Stream'
map :: (a -> b)
    -> (Multi a -> Multi b)
    -> Stream a
    -> Stream b
{-# INLINE map #-}
map = M.map

-- | Zip two 'Stream's with the given function
zipWith :: (a -> b -> c)
        -> (Multi a -> Multi b -> Multi c)
        -> Stream a
        -> Stream b
        -> Stream c
{-# INLINE zipWith #-}
zipWith = M.zipWith

-- | Left fold
foldl :: (a -> b -> a)
      -> (Multi a -> Multi b -> Multi a)
      -> (Multi a -> a)
      -> Multi a
      -> Stream b
      -> a
{-# INLINE foldl #-}
foldl f1 f red z = unId . M.foldl f1 f red z

-- | Left fold
foldl' :: (a -> b -> a)
       -> (Multi a -> Multi b -> Multi a)
       -> (Multi a -> a)
       -> Multi a
       -> Stream b
       -> a
{-# INLINE foldl' #-}
foldl' f1 f red z = unId . M.foldl' f1 f red z
