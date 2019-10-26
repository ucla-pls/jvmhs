-- |
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Jvmhs.Analysis.HierarchySpec where

import SpecHelper
import Test.QuickCheck

import Data.Binary

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

import Jvmhs.Analysis.Hierarchy
import Jvmhs

spec :: Spec
spec = do
  describe "load and save stubs" $ do
    prop "can load and save a binary file" $ \(a :: HierarchyStubs) -> do
      decode (encode a) === a

  withJREHierarchy $ do
    describe "super" $ do
      it "java/lang/Iterable super should be java/lang/Object" $ \hry -> do
        super "java/lang/Iterable" hry
        `shouldBe`
          Just (Just "java/lang/Object")

      it "java/lang/Object super should be Nothing" $ \hry -> do
        super "java/lang/Object" hry
        `shouldBe`
          Just Nothing

    describe "interfaces" $ do
      it "java/lang/Iterable should have no interfaces " $ \hry -> do
        interfaces "java/lang/Iterable" hry
        `shouldBe`
          Just []

    describe "parents" $ do
      it "java/lang/Iterable should have one parent" $ \hry -> do
        parents "java/lang/Iterable" hry
        `shouldBe`
          [ "java/lang/Object"
          ]
      it "java/lang/Object should have no parents" $ \hry -> do
        parents "java/lang/Object" hry
        `shouldBe`
          []

    describe "superclasses" $ do
      it "java/lang/String should have five superclasses" $ \hry -> do
        superclasses "java/lang/String" hry
        `shouldBe`
          [ "java/lang/String"
          , "java/lang/Object"
          , "java/lang/CharSequence"
          , "java/lang/Object"
          , "java/lang/Comparable"
          , "java/lang/Object"
          , "java/io/Serializable"
          , "java/lang/Object"
          ]


    describe "isSubclassOf" $ do
      it "should type check string as a subclass of object" $ \hry -> do
        (isSubclassOf "java/lang/String" "java/lang/Object" $ hry)
          `shouldBe` True

      it "should type check string as a subclass of string" $ \hry -> do
        (isSubclassOf' "java/lang/String" "java/lang/String" $ hry)
          `shouldBe` Just True

    describe "subclassPath" $ do
      it "can find a path from string to object" $ \hry -> do
        (head $ subclassPath "java/lang/String" "java/lang/Object" $ hry)
        `shouldBe` [ ("java/lang/String", "java/lang/Object", Extend) ]

      it "should find path from 'java/util/ArrayList' to 'java/util/List'" $ \hry -> do
        (head $ subclassPath "java/util/ArrayList" "java/util/List" $ hry)
          `shouldBe`
          [ ("java/util/ArrayList", "java/util/AbstractList", Extend)
          , ("java/util/AbstractList", "java/util/List", Implement)
          ]

      it "should find path from 'java/util/ArrayList' to 'java/lang/Object'" $ \hry -> do
        (head $ subclassPath "java/util/ArrayList" "java/lang/Object" hry)
          `shouldBe`
          [ ("java/util/ArrayList", "java/util/AbstractList", Extend)
          , ("java/util/AbstractList", "java/util/AbstractCollection", Extend)
          , ("java/util/AbstractCollection", "java/lang/Object", Extend)
          ]


instance Arbitrary HierarchyStubs where
  arbitrary = HierarchyStubs . M.fromList <$> arbitrary

instance Arbitrary HierarchyStub where
  arbitrary = HierarchyStub
    <$> arbitrary
    <*> arbitrary
    <*> (S.fromList <$> arbitrary)
    <*> (M.fromList <$> arbitrary)
    <*> (S.fromList <$> arbitrary)

instance Arbitrary ClassName where
  arbitrary = elements [ "java/lang/Object", "some/other/Name", "some/third/Name" ]

instance Arbitrary MethodId where
  arbitrary = elements [ "<init>:()V", "f:([I)I", "base:(DD)J" ]

instance Arbitrary FieldId where
  arbitrary = elements [ "field:I", "lang:Ljava/lang/Object;"]
