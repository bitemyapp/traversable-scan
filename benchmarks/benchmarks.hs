module Main where

import Control.Monad ( replicateM )
import Criterion.Main
import System.Random ( randomIO )

import qualified Data.List as L
import qualified Data.Traversable.Scan as T
import qualified Data.Vector as V

main :: IO ()
main = do
  xs <- replicateM 1000000 randomIO :: IO [Int]
  let ys = V.fromList xs
  defaultMain
    [ bench "prescanl/Traversable/[]" (nf (T.prescanl (+) 0) xs)
    , bench "prescanl/Traversable/Vector" (nf (T.prescanl (+) 0) ys)
    , bench "prescanl/Vector" (nf (V.prescanl (+) 0) ys)

    , bench "prescanl'/Traversable/[]" (nf (T.prescanl' (+) 0) xs)
    , bench "prescanl'/Traversable/Vector" (nf (T.prescanl' (+) 0) ys)
    , bench "prescanl'/Vector" (nf (V.prescanl' (+) 0) ys)

    , bgroup "postscanl"
      [ bench "Traversable []" (nf (T.postscanl (+) 0) xs)
      , bench "Traversable Vector" (nf (T.postscanl (+) 0) ys)
      , bench "Vector" (nf (V.postscanl (+) 0) ys)
      ]
    , bgroup "postscanl'"
      [ bench "Traversable []" (nf (T.postscanl' (+) 0) xs)
      , bench "Traversable Vector" (nf (T.postscanl' (+) 0) ys)
      , bench "Vector" (nf (V.postscanl' (+) 0) ys)
      ]
    , bgroup "scanl1"
      [ bench "Traversable []" (nf (T.scanl1 (+)) xs)
      , bench "Traversable Vector" (nf (T.scanl1 (+)) ys)
      , bench "[]" (nf (L.scanl1 (+)) xs)
      , bench "Vector" (nf (V.scanl1 (+)) ys)
      ]
    , bgroup "scanl1'"
      [ bench "Traversable []" (nf (T.scanl1' (+)) xs)
      , bench "Traversable Vector" (nf (T.scanl1' (+)) ys)
      , bench "Vector" (nf (V.scanl1' (+)) ys)
      ]
    , bgroup "prescanr"
      [ bench "Traversable []" (nf (T.prescanr (+) 0) xs)
      , bench "Traversable Vector" (nf (T.prescanr (+) 0) ys)
      , bench "Vector" (nf (V.prescanr (+) 0) ys)
      ]
    , bgroup "prescanr'"
      [ bench "Traversable []" (nf (T.prescanr' (+) 0) xs)
      , bench "Traversable Vector" (nf (T.prescanr' (+) 0) ys)
      , bench "Vector" (nf (V.prescanr' (+) 0) ys)
      ]
    , bgroup "postscanr"
      [ bench "Traversable []" (nf (T.postscanr (+) 0) xs)
      , bench "Traversable Vector" (nf (T.postscanr (+) 0) ys)
      , bench "Vector" (nf (V.postscanr (+) 0) ys)
      ]
    , bgroup "postscanr'"
      [ bench "Traversable []" (nf (T.postscanr' (+) 0) xs)
      , bench "Traversable Vector" (nf (T.postscanr' (+) 0) ys)
      , bench "Vector" (nf (V.postscanr' (+) 0) ys)
      ]
    , bgroup "scanr1"
      [ bench "Traversable []" (nf (T.scanr1 (+)) xs)
      , bench "Traversable Vector" (nf (T.scanr1 (+)) ys)
      , bench "[]" (nf (L.scanr1 (+)) xs)
      , bench "Vector" (nf (V.scanr1 (+)) ys)
      ]
    , bgroup "scanr1'"
      [ bench "Traversable []" (nf (T.scanr1' (+)) xs)
      , bench "Traversable Vector" (nf (T.scanr1' (+)) ys)
      , bench "Vector" (nf (V.scanr1' (+)) ys)
      ]
    , bgroup "scanl"
      [ bench "Traversable []" (nf (T.scanl (+) 0) xs)
      , bench "Traversable Vector" (nf (T.scanl (+) 0) ys)
      , bench "[]" (nf (L.scanl (+) 0) xs)
      , bench "Vector" (nf (V.scanl (+) 0) ys)
      ]
    , bgroup "scanl'"
      [ bench "Traversable []" (nf (T.scanl' (+) 0) xs)
      , bench "Traversable Vector" (nf (T.scanl' (+) 0) ys)
      , bench "Vector" (nf (V.scanl' (+) 0) ys)
      ]
    , bgroup "scanr"
      [ bench "Traversable []" (nf (T.scanr (+) 0) xs)
      , bench "Traversable Vector" (nf (T.scanr (+) 0) ys)
      , bench "[]" (nf (L.scanr (+) 0) xs)
      , bench "Vector" (nf (V.scanr (+) 0) ys)
      ]
    , bgroup "scanr'"
      [ bench "Traversable []" (nf (T.scanr' (+) 0) xs)
      , bench "Traversable Vector" (nf (T.scanr' (+) 0) ys)
      , bench "Vector" (nf (V.scanr' (+) 0) ys)
      ]
    ]
