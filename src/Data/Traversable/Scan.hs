{-# LANGUAGE UnboxedTuples #-}

module Data.Traversable.Scan where

import Data.Traversable ( mapAccumR )

-- * Left-to-right state transformer

newtype StateL s a = StateL { runStateL :: s -> (# s, a #) }

instance Functor (StateL s) where
  {-# INLINE fmap #-}
  fmap f (StateL k) =
    let
      fmap_StateL_inner s0 =
        case k s0 of (# s1, v #) -> (# s1, f v #)
    in
      StateL fmap_StateL_inner

instance Applicative (StateL s) where
  {-# INLINE pure #-}
  pure x =
    let
      pure_StateL_inner s = (# s, x #)
    in
      StateL pure_StateL_inner

  {-# INLINE (<*>) #-}
  (<*>) (StateL kf) (StateL kv) =
    let
      ap_StateL_inner s0 =
        case kf s0 of
          (# s1, f #) ->
            case kv s1 of
              (# s2, v #) -> (# s2, f v #)
    in
      StateL ap_StateL_inner

instance Monad (StateL s) where
  {-# INLINE return #-}
  return x =
    let
      return_StateL_inner s = (# s, x #)
    in
      StateL return_StateL_inner

  {-# INLINE (>>=) #-}
  (>>=) (StateL ka) fkb =
    let
      bind_StateL_inner s0 =
        case ka s0 of
          (# s1, a #) ->
            case fkb a of
              StateL kb -> kb s1
    in
      StateL bind_StateL_inner

-- |The 'mapAccumL' function behaves like a combination of 'fmap'
-- and 'foldl'; it applies a function to each element of a structure,
-- passing an accumulating parameter from left to right, and returning
-- a final value of this accumulator together with the new structure.
mapAccumL :: Traversable t => (a -> b -> (# a, c #)) -> a -> t b -> (a, t c)
{-# INLINE mapAccumL #-}
mapAccumL f = \s0 t0 ->
  case runStateL (mapM mapAccumL_go t0) s0 of
    (# s1, t1 #) -> (s1, t1)
  where
    mapAccumL_go a = StateL (\s -> f s a)

-- * Prefix sums (scans)

{-

All of the length-preserving scans can be implemented directly for
Traversable in terms of mapAccumL and mapAccumR.

In case you (like me) can never remember the difference between
prescans and postscans:
- prescans keep the initial value, but discard the final
- postscans discard the initial value, but keep the final
In other words, prescans keep the pre-value, and postscans keep
the post-value.

-}

(...) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
{-# INLINE (...) #-}
(...) f g = \c d -> f (g c d)

prescanl :: Traversable t => (a -> b -> a) -> a -> t b -> t a
prescanl f = \s t -> snd (mapAccumL go s t) where
  go a b = let c = f a b in (# c, a #)

prescanl' :: Traversable t => (a -> b -> a) -> a -> t b -> t a
prescanl' f = \s t -> snd (mapAccumL go s t) where
  go a b = let c = f a b in seq c (# c, a #)

postscanl :: Traversable t => (a -> b -> a) -> a -> t b -> t a
postscanl f = \s t -> snd (mapAccumL go s t) where
  go a b = let c = f a b in (# c, c #)

postscanl' :: Traversable t => (a -> b -> a) -> a -> t b -> t a
postscanl' f = \s t -> snd (mapAccumL go s t) where
  go a b = let c = f a b in seq c (# c, c #)

scanl1 :: Traversable t => (a -> a -> a) -> t a -> t a
scanl1 f = \t -> snd (mapAccumL go Nothing t) where
  go a b = let c = (maybe id f) a b in (# Just c, c #)

scanl1' :: Traversable t => (a -> a -> a) -> t a -> t a
scanl1' f = \t -> snd (mapAccumL go Nothing t) where
  go a b = let c = (maybe id f) a b in seq c (# Just c, c #)

prescanr :: Traversable t => (a -> b -> b) -> b -> t a -> t b
prescanr f = snd ... mapAccumR go where
  go b a = let c = f a b in (c, b)

prescanr' :: Traversable t => (a -> b -> b) -> b -> t a -> t b
prescanr' f = snd ... mapAccumR go where
  go b a = seq b (let c = f a b in (c, b))

postscanr :: Traversable t => (a -> b -> b) -> b -> t a -> t b
postscanr f = snd ... mapAccumR go where
  go b a = let c = f a b in (c, c)

postscanr' :: Traversable t => (a -> b -> b) -> b -> t a -> t b
postscanr' f = snd ... mapAccumR go where
  go b a = seq b (let c = f a b in (c, c))

scanr1 :: Traversable t => (a -> a -> a) -> t a -> t a
scanr1 f = snd . mapAccumR go Nothing where
  go b a = let c = maybe a (f a) b in (Just c, c)

scanr1' :: Traversable t => (a -> a -> a) -> t a -> t a
scanr1' f = snd . mapAccumR go Nothing where
  go b a = (maybe id seq) b (let c = maybe a (f a) b in (Just c, c))

{-

These are variants of scanl and scanr. The original versions cannot
be implemented for Traversable because they change length, but I
often find that these variants are what I actually need.

Why are these so great? They simultaneously compute the prescan
and the fold in one direction. The original versions do that as well,
but with these I can get the fold's result out without knowing anything
about the structure of the Traversable.

-}

swap :: (a, b) -> (b, a)
{-# INLINE swap #-}
swap = uncurry (flip (,))

scanl :: Traversable t => (a -> b -> a) -> a -> t b -> (t a, a)
scanl f = \s t -> swap (mapAccumL go s t) where
  go a b = let c = f a b in (# c, a #)

scanl' :: Traversable t => (a -> b -> a) -> a -> t b -> (t a, a)
scanl' f = \s t -> swap (mapAccumL go s t) where
  go a b = let c = f a b in seq c (# c, a #)

scanr :: Traversable t => (a -> b -> b) -> b -> t a -> (b, t b)
scanr f = mapAccumR go where
  go b a = let c = f a b in (c, b)

scanr' :: Traversable t => (a -> b -> b) -> b -> t a -> (b, t b)
scanr' f = mapAccumR go where
  go b a = seq b (let c = f a b in (c, b))
