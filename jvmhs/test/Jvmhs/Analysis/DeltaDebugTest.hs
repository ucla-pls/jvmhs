{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Jvmhs.Analysis.DeltaDebugTest where

import SpecHelper
import Jvmhs

import Data.Monoid

import Control.Monad.Writer.Class

import qualified Data.Vector as V


spec_binarySearch :: Spec
spec_binarySearch = do
  let l100 = (V.fromList ([0..100] :: [Int]))
  it "can do a binary search on a list" $ do
    binarySearch (\s -> tell [s] >> pure (s >= 23)) l100
      `shouldBe`
      ([50, 25, 12, 19, 22, 24, 23], Just 23)

  it "returns the smallest element if true" $ do
    binarySearch (\_ -> pure True) l100 `shouldBe` Identity (Just 0)

  it "returns Nothing if false" $ do
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

-- spec_gdd :: Spec
-- spec_gdd = do
--   -- it "can find a minimal closure for k = 0" $ do
--   --   gdd (em $ const True) graph
--   --     `shouldBe`
--   --     ((), ([] :: [Int]))
--   it "can find a minimal closure for k = 1" $ do
--     gdd (em $ any (== 2)) graph
--       `shouldBe`
--       ((), ([2, 3, 4] :: [Int]))
--   it "can find a minimal closure for k = 2" $ do
--     gdd(em $ (\s -> any (== 3) s && any (== 4) s)) graph
--       `shouldBe`
--       ((), ([3, 4] :: [Int]))


test8 :: [Int]
test8 = [1..8]

is7 :: Foldable t => t Int -> Bool
is7 v =
  elem (7 :: Int) v

is167 :: Foldable t => t Int -> Bool
is167 v =
  and [(elem 1 v), (elem 6 v), (elem (7 :: Int) v)]

is178 :: Foldable t => t Int -> Bool
is178 v =
  and [(elem 1 v), (elem 7 v), (elem (8 :: Int) v)]

is1234 :: Foldable t => t Int -> Bool
is1234 v =
  and [(elem 1 v), (elem 2 v), (elem 3 v), (elem 4 v)]

is28 :: Foldable t => t Int -> Bool
is28 v =
  and [(elem 2 v), (elem (8 :: Int) v)]


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
  it "can solve a simple case " $ do
    sdd (count is7) test8 `shouldBe` (Sum 5, [7])

  it "can solve the dd-min case" $ do
    sdd (count is178) test8 `shouldBe` (Sum 18, [1,7,8])

  it "can solve a k=2 case " $ do
    sdd (count (\s -> is167 s || is28 s)) test8 `shouldBe` (Sum 25, [2,8])

  it "returns a empty element if true" $
    sdd (count $ const True) test8 `shouldBe` (Sum 1, [])

  it "returns everything if false" $
    sdd (count $ const False) test8 `shouldBe` (Sum 4, test8)

  it "does find an optimal solution for some cases" $ do
    sdd (count (\s -> is1234 s || is7 s)) test8 `shouldBe` (Sum 19, [7])

spec_ddmin :: Spec
spec_ddmin = do
  it "is be able to find elem with size 1" $
    ddmin (count is7) test8 `shouldBe` (Sum 5, [7])

  it "is be able to find [1,7,8]" $
    ddmin (count is178) test8 `shouldBe` (Sum 31, [1,7,8])

  it "returns a single element if true" $
    ddmin (count $ const True) test8 `shouldBe` (Sum 3, [1])

  it "does find an optimal solution for some cases" $ do
    ddmin (count (\s -> is167 s || is28 s)) test8 `shouldBe` (Sum 33, [2,8])

  it "does not find an optimal solution for some cases" $ do
    ddmin (count (\s -> is1234 s || is7 s)) test8 `shouldBe` (Sum 13, [1,2,3,4])

  it "returns everything if false" $
    ddmin (count $ const False) test8 `shouldBe` (Sum 28, test8)
