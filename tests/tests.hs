module Main where

import Data.Traversable.Scan as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Linear (M22, V2(..), (!*!))

import Test.Tasty
import Test.Tasty.QuickCheck

main :: IO ()
main = (defaultMain . testGroup "tests")
       [ testProperty "prescanl == Data.Vector.prescanl"
         (testPrePostScan T.prescanl V.prescanl)
       , testProperty "prescanl' == Data.Vector.prescanl'"
         (testPrePostScan T.prescanl' V.prescanl')
       , testProperty "postscanl == Data.Vector.postscanl"
         (testPrePostScan T.postscanl V.postscanl)
       , testProperty "postscanl' == Data.Vector.postscanl'"
         (testPrePostScan T.postscanl' V.postscanl')
       , testProperty "prescanr == Data.Vector.prescanr"
         (testPrePostScan T.prescanr V.prescanr)
       , testProperty "prescanr' == Data.Vector.prescanr'"
         (testPrePostScan T.prescanr' V.prescanr')
       , testProperty "postscanr == Data.Vector.postscanr"
         (testPrePostScan T.postscanr V.postscanr)
       , testProperty "postscanr' == Data.Vector.postscanr'"
         (testPrePostScan T.postscanr' V.postscanr')

       , testProperty "scanl1 == Data.Vector.scanl1"
         (testScan1 T.scanl1 V.scanl1)
       , testProperty "scanl1' == Data.Vector.scanl1'"
         (testScan1 T.scanl1' V.scanl1')
       , testProperty "scanr1 == Data.Vector.scanr1"
         (testScan1 T.scanr1 V.scanr1)
       , testProperty "scanr1' == Data.Vector.scanr1'"
         (testScan1 T.scanr1' V.scanr1')

       , testProperty "scanl == Data.Vector.scanl"
         (testScanL T.scanl V.scanl)
       , testProperty "scanl' == Data.Vector.scanl'"
         (testScanL T.scanl' V.scanl')

       , testProperty "scanr == Data.Vector.scanr"
         (testScanR T.scanr V.scanr)
       , testProperty "scanr' == Data.Vector.scanr'"
         (testScanR T.scanr' V.scanr')
       ]

type PrePostScan a = (a -> a -> a) -> a -> Vector a -> Vector a

testPrePostScan :: PrePostScan (M22 Double) -> PrePostScan (M22 Double) -> Gen Property
testPrePostScan f g = do
  xs <- V.fromList <$> listOf arbitraryM22
  z <- arbitraryM22
  pure (f (!*!) z xs === g (!*!) z xs)

type Scan1 a = (a -> a -> a) -> Vector a -> Vector a

testScan1 :: Scan1 (M22 Double) -> Scan1 (M22 Double) -> Gen Property
testScan1 f g = do
  xs <- V.fromList <$> listOf1 arbitraryM22
  pure (f (!*!) xs === g (!*!) xs)

type ScanL a = (a -> a -> a) -> a -> Vector a -> (Vector a, a)
type ScanL_ a = (a -> a -> a) -> a -> Vector a -> Vector a

testScanL :: ScanL (M22 Double) -> ScanL_ (M22 Double) -> Gen Property
testScanL f g = do
  xs <- V.fromList <$> listOf arbitraryM22
  z <- arbitraryM22
  let
    (ys, yf) = f (!*!) z xs
    ys' = g (!*!) z xs
  pure ((ys === V.init ys') .&&. (yf === V.last ys'))

type ScanR a = (a -> a -> a) -> a -> Vector a -> (a, Vector a)
type ScanR_ a = (a -> a -> a) -> a -> Vector a -> Vector a

testScanR :: ScanR (M22 Double) -> ScanR_ (M22 Double) -> Gen Property
testScanR f g = do
  xs <- V.fromList <$> listOf arbitraryM22
  z <- arbitraryM22
  let
    (yi, ys) = f (!*!) z xs
    ys' = g (!*!) z xs
  pure ((yi === V.head ys') .&&. (ys === V.tail ys'))

arbitraryM22 :: Gen (M22 Double)
arbitraryM22 = V2 <$> arbitraryV2 <*> arbitraryV2
  where
    arbitraryV2 = V2 <$> arbitrary <*> arbitrary
