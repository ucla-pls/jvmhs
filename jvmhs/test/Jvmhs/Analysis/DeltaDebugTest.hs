{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Jvmhs.Analysis.DeltaDebugTest where

import SpecHelper
import Jvmhs

import Data.Monoid
import Data.Maybe
import Data.List

import Control.Monad.Writer.Class

import qualified Data.Vector as V
import qualified Data.IntSet as IS


spec_binarySearch :: Spec
spec_binarySearch = do
  let l100 = V.fromList ([0..100] :: [Int])
  it "can do a binary search on a list" $
    binarySearch (\s -> tell [s] >> pure (s >= 23)) l100
      `shouldBe`
      ([50, 25, 12, 19, 22, 24, 23], Just 23)

  it "returns the smallest element if true" $
    binarySearch (\_ -> pure True) l100 `shouldBe` Identity (Just 0)

  it "returns Nothing if false" $
    binarySearch (\_ -> pure False) l100 `shouldBe` Identity Nothing



em :: MonadWriter () m => (a -> Bool) -> a -> m Bool
em f = return . f


test8 :: [Int]
test8 = [1..8]

is7 :: [Int] -> Bool
is7 = elem 7

is167 :: [Int] -> Bool
is167 = isSubsequenceOf [1, 6, 7]

is178 :: [Int] -> Bool
is178 = isSubsequenceOf [1, 7, 8]

is1234 :: [Int] -> Bool
is1234 = isSubsequenceOf [1, 2, 3, 4]

is28 :: [Int] -> Bool
is28 = isSubsequenceOf [2, 8]

count :: MonadWriter (Sum Int) m => (a -> Bool) -> a -> m Bool
count f s = do
  tell $ Sum 1
  return $ f s

listt :: MonadWriter [a] m => (a -> Bool) -> a -> m Bool
listt f s = do
  tell [s]
  return $ f s

printt :: Show a => (a -> Bool) -> a -> IO Bool
printt f s = do
  let t = f s
  print (s, t)
  return t

spec_zdd :: Spec
spec_zdd = do
  it "can solve a simple case " $
    zdd (count is7) test8 `shouldBe` (Sum 5, [7])

  it "can solve the dd-min case" $
    zdd (count is178) test8 `shouldBe` (Sum 18, [1,7,8])

  it "can solve a k=2 case " $
    zdd (count (\s -> is167 s || is28 s)) test8 `shouldBe` (Sum 25, [2,8])

  it "returns a empty element if true" $
    zdd (count $ const True) test8 `shouldBe` (Sum 1, [])

  it "returns everything if false" $
    zdd (count $ const False) test8 `shouldBe` (Sum 4, test8)

  it "does find an optimal solution for all cases" $
    zdd (count (\s -> is1234 s || is7 s)) test8 `shouldBe` (Sum 19, [7])

  it "can find half" $
    zdd (count (isSubsequenceOf [0..50])) [0..100] `shouldBe` (Sum 474, [0..50] :: [Int])

  it "can find the even halves" $
    zdd (count $ isSubsequenceOf ([0,2..100] :: [Int])) [0..100] `shouldBe` (Sum 569, [0,2..100])

spec_zsdd :: Spec
spec_zsdd = do
  it "can handle overlapping sets" $
    zsdd (em $ IS.isSubsetOf (IS.fromList [1,2,3,4]))
     [ IS.fromList i | i <- [[1,5],[1,2,3],[2,3,4]]] `shouldBe` ((), IS.fromList [1,2,3,4])

spec_isdd :: Spec
spec_isdd = do
  it "can handle overlapping sets" $
    isdd (em $ IS.isSubsetOf (IS.fromList [1,2,3,4]))
     [ IS.fromList i | i <- [[1,5],[1,2,3],[2,3,4]]] `shouldBe` ((), IS.fromList [1,2,3,4])

spec_idd :: Spec
spec_idd = do
  it "can solve a simple case simple-idd" $
    idd (count is7) test8 `shouldBe` (Sum 4, [7])

  it "can solve the dd-min case" $
    idd (count is178) test8 `shouldBe` (Sum 10, [1,7,8])

  it "can solve a k=2 case " $
    idd (count (\s -> is167 s || is28 s)) test8 `shouldBe` (Sum 16, [2,8])

  it "returns a empty element if true" $ do
    idd (count $ const True) test8 `shouldBe` (Sum 3, [1])

  it "returns everything if false" $
    idd (count $ const False) test8 `shouldBe` (Sum 24, test8)

  it "does find an optimal solution for all cases" $
    idd (count (\s -> is1234 s || is7 s)) test8 `shouldBe` (Sum 13, [7])

  -- it "can find half" $ do
  --   res <- idd (printt (isSubsequenceOf [0..25])) [0..50]
  --   res `shouldBe` ([0..25] :: [Int])

  it "can find half" $
    idd (count (isSubsequenceOf [0..50])) [0..100] `shouldBe` (Sum 298, [0..50] :: [Int])

  it "can find the even halves" $
    idd (count $ isSubsequenceOf ([0,2..100] :: [Int])) [0..100] `shouldBe` (Sum 393, [0,2..100])

spec_iidd :: Spec
spec_iidd = do
  it "can solve a simple case simple-iidd" $
    iidd (count is7) test8 `shouldBe` (Sum 4, [7])

  it "can solve the dd-min case" $
    iidd (count is178) test8 `shouldBe` (Sum 10, [1,7,8])

  it "can solve a k=2 case " $
    iidd (count (\s -> is167 s || is28 s)) test8 `shouldBe` (Sum 16, [2,8])

  it "returns a empty element if true" $ do
    iidd (count $ const True) test8 `shouldBe` (Sum 3, [1])

  it "returns everything if false" $
    iidd (count $ const False) test8 `shouldBe` (Sum 24, test8)

  it "does find an optimal solution for all cases" $
    iidd (count (\s -> is1234 s || is7 s)) test8 `shouldBe` (Sum 13, [7])

  -- it "can find half" $ do
  --   res <- iidd (printt (isSubsequenceOf [0..25])) [0..50]
  --   res `shouldBe` ([0..25] :: [Int])

  it "can find half" $
    iidd (count (isSubsequenceOf [0..50])) [0..100] `shouldBe` (Sum 298, [0..50] :: [Int])

  it "can find the even halves" $
    iidd (count $ isSubsequenceOf ([0,2..100] :: [Int])) [0..100] `shouldBe` (Sum 393, [0,2..100])


spec_ddmin :: Spec
spec_ddmin = do
  it "is be able to find elem with size 1" $
    ddmin (count is7) test8 `shouldBe` (Sum 5, [7])

  it "is be able to find [1,7,8]" $
    ddmin (count is178) test8 `shouldBe` (Sum 31, [1,7,8])

  it "returns a single element if true" $
    ddmin (count $ const True) test8 `shouldBe` (Sum 3, [1])

  it "does find an optimal solution for some cases" $
    ddmin (count (\s -> is167 s || is28 s)) test8 `shouldBe` (Sum 33, [2,8])

  it "does not find an optimal solution for some cases" $
    ddmin (count (\s -> is1234 s || is7 s)) test8 `shouldBe` (Sum 13, [1,2,3,4])

  it "returns everything if false" $
    ddmin (count $ const False) test8 `shouldBe` (Sum 28, test8)

  it "can find half" $
    ddmin (count (isSubsequenceOf [0..50])) [0..100] `shouldBe` (Sum 209, [0..50] :: [Int])

  it "can find the even halves" $
    ddmin (count $ isSubsequenceOf ([0,2..100] :: [Int])) [0..100] `shouldBe` (Sum 5464, [0,2..100])

graph :: Graph Int ()
graph =
  mkGraphFromEdges
  [ (1, 2, ())
  , (2, 3, ())
  , (2, 4, ())
  ]

spec_zgdd :: Spec
spec_zgdd = do
  it "can find a minimal closure for k = 0" $
    zgdd (count $ const True) graph
      `shouldBe`
      (Sum 1, [] :: [Int])
  it "can find a minimal closure for k = 1" $
    zgdd (count $ elem 2) graph
      `shouldBe`
      (Sum 4, [2, 3, 4] :: [Int])
  it "can find a minimal closure for k = 2" $
    zgdd(count $ \s -> elem 3 s && elem 4 s) graph
      `shouldBe`
      (Sum 7, [3, 4] :: [Int])

  it "works on more ./simple-graph.txt" $ do
    gr <- graphFromFile "test/data/graphs/simple-graph.txt"
    zgdd (count $ isSubsequenceOf [4, 6]) gr
     `shouldBe`
     (Sum 11, [3, 4, 6, 7, 8] :: [Int])

  it "works on more ./ran-2000-2000.txt" $ do
    gr <- graphFromFile "benchmark/data/ran-2000-2000.txt"
    let (s, x) = zgdd (count $ isSubsequenceOf [1]) gr
    length x `shouldBe` 22
    s `shouldBe` Sum 13

spec_igdd :: Spec
spec_igdd = do
  it "can find a minimal closure for k = 1" $
    igdd (count $ elem 2) graph
      `shouldBe`
      (Sum 4, [2, 3, 4] :: [Int])
  it "can find a minimal closure for k = 2" $
    igdd(count $ \s -> elem 3 s && elem 4 s) graph
      `shouldBe`
      (Sum 4, [3, 4] :: [Int])
  it "works on more ./simple-graph.txt" $ do
    gr <- graphFromFile "test/data/graphs/simple-graph.txt"
    igdd (count $ isSubsequenceOf [4, 6]) gr
     `shouldBe`
     (Sum 9, [3, 4, 6, 7, 8] :: [Int])

  it "works on more ./ran-2000-2000.txt" $ do
    gr <- graphFromFile "benchmark/data/ran-2000-2000.txt"
    let (s, x) = igdd (count $ isSubsequenceOf [1]) gr
    length x `shouldBe` 22
    s `shouldBe` Sum 18

spec_gddmin :: Spec
spec_gddmin = do
  it "can find a minimal closure for k = 1" $
    gddmin (count $ elem 2) graph
      `shouldBe`
      (Sum 8, [2, 3, 4] :: [Int])
  it "can find a minimal closure for k = 2" $
    gddmin (count $ \s -> elem 3 s && elem 4 s) graph
      `shouldBe`
      (Sum 5, [3, 4] :: [Int])

  it "works on more ./simple-graph.txt" $ do
    gr <- graphFromFile "test/data/graphs/simple-graph.txt"
    gddmin (count $ isSubsequenceOf [4, 6]) gr
     `shouldBe`
     (Sum 17, [3, 4, 6, 7, 8] :: [Int])
