{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.JVM.Binary.ClassFileTest where

import SpecHelper

import qualified Data.IntMap as IM

import Language.JVM.Binary.ClassFile
import Language.JVM.Binary.Constant

import Language.JVM.Binary.SizedList
import Language.JVM.Binary.SizedListTest ()
import Language.JVM.Binary.AttributeTest ()
import Language.JVM.Binary.ConstantTest ()
import Language.JVM.Binary.FieldTest ()
import Language.JVM.Binary.MethodTest ()

spec_reading_classfile :: Spec
spec_reading_classfile = do
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
    <*> (pure $ SizedList16 [])
    <*> (pure $ SizedList16 [])
    <*> (pure $ SizedList16 [])
    <*> (pure $ SizedList16 [])

instance Arbitrary AccessFlags where
  arbitrary = AccessFlags <$> arbitrary

instance Arbitrary AccessFlag where
  arbitrary = toEnum <$> choose (0, 15)
