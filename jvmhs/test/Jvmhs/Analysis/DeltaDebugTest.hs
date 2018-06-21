{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Jvmhs.Analysis.DeltaDebugTest where

import SpecHelper
import Jvmhs

import Data.Monoid
import Data.List

import Control.Monad.Writer.Class

import qualified Data.Vector as V


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


graph :: Graph Int ()
graph =
  mkGraphFromEdges
  [ (1, 2, ())
  , (2, 3, ())
  , (2, 4, ())
  ]

em :: MonadWriter () m => ([v] -> Bool) -> [v] -> m Bool
em f = return . f

spec_gdd :: Spec
spec_gdd = do
  it "can find a minimal closure for k = 0" $
    gdd (em $ const True) graph
      `shouldBe`
      ((), [] :: [Int])
  it "can find a minimal closure for k = 1" $
    gdd (em $ elem 2) graph
      `shouldBe`
      ((), [2, 3, 4] :: [Int])
  it "can find a minimal closure for k = 2" $
    gdd(em $ \s -> elem 3 s && elem 4 s) graph
      `shouldBe`
      ((), [3, 4] :: [Int])


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

count :: MonadWriter (Sum Int) m => ([v] -> Bool) -> [v] -> m Bool
count f s = do
  tell $ Sum 1
  return $ f s

listt :: MonadWriter [[v]] m => ([v] -> Bool) -> [v] -> m Bool
listt f s = do
  tell [s]
  return $ f s

spec_sdd :: Spec
spec_sdd = do
  it "can solve a simple case " $
    sdd (count is7) test8 `shouldBe` (Sum 5, [7])

  it "can solve the dd-min case" $
    sdd (count is178) test8 `shouldBe` (Sum 18, [1,7,8])

  it "can solve a k=2 case " $
    sdd (count (\s -> is167 s || is28 s)) test8 `shouldBe` (Sum 25, [2,8])

  it "returns a empty element if true" $
    sdd (count $ const True) test8 `shouldBe` (Sum 1, [])

  it "returns everything if false" $
    sdd (count $ const False) test8 `shouldBe` (Sum 4, test8)

  it "does find an optimal solution for some cases" $
    sdd (count (\s -> is1234 s || is7 s)) test8 `shouldBe` (Sum 19, [7])

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
