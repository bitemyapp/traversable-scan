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

prescanl :: Traversable t => (a -> b -> a) -> a -> t b -> t a
prescanl f z bs = snd (mapAccumL go z bs) where
  go a b = let c = f a b in (a, c)

prescanl' :: Traversable t => (a -> b -> a) -> a -> t b -> t a
prescanl' f z bs = snd (mapAccumL go z bs) where
  go a b = seq a (let c = f a b in (a, c))

postscanl :: Traversable t => (a -> b -> a) -> a -> t b -> t a
postscanl f z bs = snd (mapAccumL go z bs) where
  go a b = let c = f a b in (c, c)

postscanl' :: Traversable t => (a -> b -> a) -> a -> t b -> t a
postscanl' f z bs = snd (mapAccumL go z bs) where
  go a b = seq a (let c = f a b in (c, c))

scanl1 :: Traversable t => (a -> a -> a) -> t a -> t a
scanl1 f as = snd (mapAccumL go Nothing as) where
  go a b = let c = (maybe id f) a b in (c, c)

scanl1' :: Traversable t => (a -> a -> a) -> t a -> t a
scanl1' f as = snd (mapAccumL go Nothing as) where
  go a b = seq a (let c = (maybe id f) a b in (c, c))

prescanr :: Traversable t => (a -> b -> b) -> b -> t a -> t b
prescanr f z bs = snd (mapAccumR go z bs) where
  go a b = let c = f a b in (a, c)

prescanr' :: Traversable t => (a -> b -> b) -> b -> t a -> t b
prescanr' f z bs = snd (mapAccumR go z bs) where
  go a b = seq b (let c = f a b in (a, c))

postscanr :: Traversable t => (a -> b -> b) -> b -> t a -> t b
postscanr f z bs = snd (mapAccumR go z bs) where
  go a b = let c = f a b in (c, c)

postscanr' :: Traversable t => (a -> b -> b) -> b -> t a -> t b
postscanr' f z bs = snd (mapAccumR go z bs) where
  go a b = seq b (let c = f a b in (c, c))

scanr1 :: Traversable t => (a -> a -> a) -> t a -> t a
scanr1 f as = snd (mapAccumR go Nothing as) where
  go a b = let c = (maybe id f) a b in (c, c)

scanr1' :: Traversable t => (a -> a -> a) -> t a -> t a
scanr1' f as = snd (mapAccumR go Nothing as) where
  go a b = seq b (let c = (maybe id f) a b in (c, c))

{-

These are variants of scanl and scanr. The original versions cannot
be implemented for Traversable because they change length, but I
often find that these variants are what I actually need.

Why are these so great? They simultaneously compute the prescan
and the fold in one direction. The original versions do that as well,
but with these I can get the fold's result out without knowing anything
about the structure of the Traversable.

-}

scanl :: Traversable t => (a -> b -> a) -> a -> t b -> (t a, a)

scanl' :: Traversable t => (a -> b -> a) -> a -> t b -> (t a, a)

scanr :: Traversable t => (a -> b -> b) -> b -> t a -> (b, t b)

scanr' :: Traversable t => (a -> b -> b) -> b -> t a -> (b, t b)
