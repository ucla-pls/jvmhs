-- |
{-# LANGUAGE OverloadedStrings #-}

module Jvmhs.Analysis.HierarchySpec where

import SpecHelper

import Jvmhs.Analysis.Hierarchy

spec :: Spec
spec = do
  (runIO (getJREHierachy [] "stdlib-stubs.json") >>=) . mapM_ $ \hry -> do
    describe "subclassPath" $ do
      it "should find path from 'java/util/ArrayList' to 'java/util/List'" $ do
        subclassPath hry "java/util/ArrayList" "java/util/List"
          `shouldBe` Just
          [ ("java/util/ArrayList", "java/util/AbstractList", Extend)
          , ("java/util/AbstractList", "java/util/List", Implement)
          ]

      it "should find path from 'java/util/ArrayList' to 'java/lang/Object'" $ do
        subclassPath hry "java/util/ArrayList" "java/lang/Object"
          `shouldBe` Just
          [ ("java/util/ArrayList", "java/util/AbstractList", Extend)
          , ("java/util/AbstractList", "java/util/AbstractCollection", Extend)
          , ("java/util/AbstractCollection", "java/lang/Object", Extend)
          ]
