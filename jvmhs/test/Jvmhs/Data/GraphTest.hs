{-# LANGUAGE OverloadedStrings #-}
module Jvmhs.Data.GraphTest where

import SpecHelper
import Jvmhs

graph :: Graph Int ()
graph =
  mkGraphFromEdges
  [ (1, 2, ())
  , (2, 3, ())
  , (2, 4, ())
  ]

spec_partition :: Spec
spec_partition = do
  it "can handle the simple graph" $ do
    partition graph `shouldBe` [ ([4], [4]), ([3], [3]), ([2], [2, 3, 4]), ([1], [1, 2, 3, 4]) ]
