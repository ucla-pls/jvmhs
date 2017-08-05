{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.JVM.Binary.ClassFileTest where

import SpecHelper

import qualified Data.Vector as V
import qualified Data.IntMap as IM

import Language.JVM.Binary.ClassFile
import Language.JVM.Binary.Constant

import Language.JVM.Binary.AttributeTest ()
import Language.JVM.Binary.ConstantTest ()

spec_Reading_classfile :: Spec
spec_Reading_classfile = do
  beforeAll (blReadFile "test-suite/project/Main.class") $ do
    it "can read the bytestring" $ \bs ->
      let classfile = decode bs
      in (magicNumber classfile) `shouldBe` 3405691582

prop_encode_and_decode :: ClassFile -> Bool
prop_encode_and_decode attr =
  (decode . encode) attr == (attr :: ClassFile)

instance Arbitrary ClassFile where
  arbitrary = ClassFile
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (pure $ ConstantPool IM.empty)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (pure $ V.empty)
    <*> (pure $ V.empty)
    <*> (pure $ V.empty)
    <*> (pure $ V.empty)

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = do
    x <- choose (0, 5)
    V.replicateM x arbitrary

instance Arbitrary AccessFlags where
  arbitrary = AccessFlags <$> arbitrary

instance Arbitrary AccessFlag where
  arbitrary = toEnum <$> choose (0, 15)
