{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Jvmhs.Data.TypeSpec where

import           SpecHelper

import           Test.QuickCheck

import           Jvmhs.Data.Type
import           Jvmhs.Data.Annotation

spec :: Spec
spec = do
  describe "ClassType" $ do
    let ct = ClassType
          "package/Name"
          (Just (ClassType "Inner" Nothing [] emptyTypeAnnotation))
          []
          emptyTypeAnnotation
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
  , RefArrayType <$> genType
  ]

genClassType :: Gen ClassType
genClassType =
  scale (`div` 2)
    $   ClassType
    <$> elements ["A", "pkg/A", "a/B"]
    <*> genInnerClassType
    <*> listOf genTypeArgument
    <*> pure emptyTypeAnnotation
 where
  genInnerClassType = sized \case
    0 -> pure Nothing
    n ->
      resize (n `div` 2)
        .   fmap Just
        $   ClassType
        <$> elements ["In1", "In2"]
        <*> genInnerClassType
        <*> listOf genTypeArgument
        <*> pure emptyTypeAnnotation

genTypeArgument :: Gen TypeArgument
genTypeArgument =
  oneof [pure AnyType, TypeArgument <$> genTypeArgumentDescription]

genTypeArgumentDescription :: Gen TypeArgumentDescription
genTypeArgumentDescription =
  TypeArgumentDescription
    <$> (oneof [Just <$> genWildcard, pure Nothing])
    <*> genReferenceType

genWildcard :: Gen Wildcard
genWildcard = genericArbitraryU

genType :: Gen Type
genType = oneof [ReferenceType <$> genReferenceType, BaseType <$> genBaseType]

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
