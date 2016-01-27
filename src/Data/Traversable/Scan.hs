module Data.Traversable.Scan where

import Data.Traversable

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
prescanl f = snd ... mapAccumL go where
  go a b = let c = f a b in (c, a)

prescanl' :: Traversable t => (a -> b -> a) -> a -> t b -> t a
prescanl' f = snd ... mapAccumL go where
  go a b = seq a (let c = f a b in (c, a))

postscanl :: Traversable t => (a -> b -> a) -> a -> t b -> t a
postscanl f = snd ... mapAccumL go where
  go a b = let c = f a b in (c, c)

postscanl' :: Traversable t => (a -> b -> a) -> a -> t b -> t a
postscanl' f = snd ... mapAccumL go where
  go a b = seq a (let c = f a b in (c, c))

scanl1 :: Traversable t => (a -> a -> a) -> t a -> t a
scanl1 f = snd . mapAccumL go Nothing where
  go a b = let c = (maybe id f) a b in (Just c, c)

scanl1' :: Traversable t => (a -> a -> a) -> t a -> t a
scanl1' f = snd . mapAccumL go Nothing where
  go a b = (maybe id seq) a (let c = (maybe id f) a b in (Just c, c))

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
scanl f = swap ... mapAccumL go where
  go a b = let c = f a b in (c, a)

scanl' :: Traversable t => (a -> b -> a) -> a -> t b -> (t a, a)
scanl' f = swap ... mapAccumL go where
  go a b = seq a (let c = f a b in (c, a))

scanr :: Traversable t => (a -> b -> b) -> b -> t a -> (b, t b)
scanr f = mapAccumR go where
  go b a = let c = f a b in (c, b)

scanr' :: Traversable t => (a -> b -> b) -> b -> t a -> (b, t b)
scanr' f = mapAccumR go where
  go b a = seq b (let c = f a b in (c, b))
