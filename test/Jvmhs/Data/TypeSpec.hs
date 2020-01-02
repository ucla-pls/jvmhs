{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Jvmhs.Data.TypeSpec where

import           SpecHelper

import           Test.QuickCheck

import qualified Data.HashMap.Strict           as HashMap

import qualified Language.JVM                  as B

import           Jvmhs.Data.Type
import           Jvmhs.Data.Annotation

import           Jvmhs.Data.AnnotationSpec      ( )


spec :: Spec
spec = do
  describe "ClassType" $ do
    let ct = ClassType
          "package/Name"
          (Just (withNoAnnotation $ ClassType "Inner" Nothing []))
          []
    it "an inner class should be inner" $ do
      classNameFromType ct `shouldBe` "package/Name$Inner"

    it "should produce a classname" $ do
      classTypeFromName "package/Name$Inner" `shouldBe` ct

    prop "a classtype should always produce the same ClassName"
      $ \cn -> classNameFromType (classTypeFromName cn) `shouldBe` cn


-- * Generators

genBaseType :: Gen JBaseType
genBaseType = genericArbitraryU

genReferenceType :: Gen ReferenceType
genReferenceType = oneof
  [ RefClassType <$> genClassType
  , RefTypeVariable <$> genTypeVariable
  , RefArrayType <$> genArrayType
  ]

genClassType :: Gen ClassType
genClassType =
  scale (`div` 2)
    $   ClassType
    <$> elements ["A", "pkg/A", "a/B"]
    <*> genInnerClassType
    <*> listOf (genAnnotated genTypeArgument)
 where
  genInnerClassType = sized \case
    0 -> pure Nothing
    n ->
      resize (n `div` 2)
        .   fmap Just
        .   genAnnotated
        $   ClassType
        <$> elements ["In1", "In2"]
        <*> genInnerClassType
        <*> listOf (genAnnotated genTypeArgument)

genAnnotated :: Gen a -> Gen (Annotated a)
genAnnotated f = Annotated <$> f <*> genAnnotations

genTypeArgument :: Gen TypeArgument
genTypeArgument = scale (`div` 2) $ oneof
  [ pure AnyTypeArg
  , TypeArg <$> genReferenceType
  , ExtendedTypeArg <$> genAnnotated genReferenceType
  , ImplementedTypeArg <$> genAnnotated genReferenceType
  ]

genType :: Gen Type
genType = oneof [ReferenceType <$> genReferenceType, BaseType <$> genBaseType]

genArrayType :: Gen ArrayType
genArrayType = ArrayType <$> genAnnotated genType

genTypeVariable :: Gen TypeVariable
genTypeVariable = TypeVariable <$> elements ["A", "B", "T"]

instance Arbitrary TypeVariable where
  arbitrary = genTypeVariable

instance Arbitrary ClassName where
  arbitrary =
    elements ["JustClass", "more/ClassName", "with/inner/Class$className"]

instance Arbitrary JRefType where
  arbitrary = genericArbitraryU

instance Arbitrary JBaseType where
  arbitrary = genericArbitraryU

instance Arbitrary JType where
  arbitrary = genericArbitraryU

instance Arbitrary B.FieldDescriptor where
  arbitrary = elements ["I", "Ljava/lang/String;"]

instance Arbitrary B.ReturnDescriptor where
  arbitrary = genericArbitraryU

genAnnotation :: Gen Annotation
genAnnotation =
  scale (`div` 2)
    $   Annotation
    <$> elements ["an/annotations/Ano"]
    <*> arbitrary
    <*> genAnnotationMap

genAnnotationMap :: Gen AnnotationMap
genAnnotationMap = HashMap.fromList
  <$> listOf ((,) <$> elements ["value", "item", "x"] <*> genAnnotationValue)

genAnnotations :: Gen Annotations
genAnnotations = listOf genAnnotation

genAnnotationValue :: Gen AnnotationValue
genAnnotationValue = scale (`div` 2) $ oneof
  [ AByte <$> arbitrary
  , AChar <$> arbitrary
  , ADouble <$> arbitrary
  , AFloat <$> arbitrary
  , AInt <$> arbitrary
  , ALong <$> arbitrary
  , AShort <$> arbitrary
  , ABoolean <$> arbitrary
  , AString <$> elements ["some", "wierd", "strings"]
  , AEnum <$> arbitrary
  , AClass <$> genericArbitraryU
  , AAnnotation <$> ((,) <$> elements ["a", "b", "c"] <*> genAnnotationMap)
  , AArray <$> arbitrary
  ]

instance Arbitrary Annotation where
  arbitrary = genAnnotation

instance Arbitrary AnnotationValue where
  arbitrary = genAnnotationValue

instance Arbitrary (EnumValue B.High) where
  arbitrary = EnumValue <$> arbitrary <*> elements ["one", "two", "three"]
