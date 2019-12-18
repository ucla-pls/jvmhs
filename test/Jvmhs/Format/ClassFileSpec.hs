{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Jvmhs.Format.ClassFileSpec where

import           SpecHelper

import           Test.QuickCheck

import           Control.Monad

import qualified Data.Set                      as Set

-- generic-random
import           Generic.Random

import qualified Language.JVM                  as B

import           Jvmhs.Format.ClassFile
import           Jvmhs.Data.Class
import           Jvmhs.Data.Annotation

spec :: Spec
spec = do
  spec_fieldAttributes
  spec_field

spec_fieldAttributes :: Spec
spec_fieldAttributes = describe "fieldAttributeFormat" $ do
  test_formatter genFieldAttributes (flipDirection fieldAttributesFormat)

 where
  genFieldAttributes = do
    value       <- pure (Just $ VInteger 0)
    annotations <- pure emptyAnnotations
    tpe         <- genType

    pure (value, annotations, tpe)


spec_field :: Spec
spec_field = describe "fieldFormat" $ do
  test_formatter genField (flipDirection fieldFormat)

 where

  genField = do
    _fieldName        <- pure "field"
    _fieldType        <- genType
    _fieldAccessFlags <- pure (Set.empty)
    _fieldValue       <- pure Nothing
    _fieldAnnotations <- pure emptyAnnotations
    pure Field { .. }




test_formatter :: (Eq a, Eq b, Show b, Show a) => Gen a -> Formatter a b -> Spec
test_formatter gen PartIso { there, back } =
  prop "should be an adjunction" . forAll gen $ \a ->
    (there >=> back >=> there) a === there a


genTypeVariable :: Gen TypeVariable
genTypeVariable = TypeVariable <$> elements ["A", "B", "T"]

instance Arbitrary TypeVariable where
  arbitrary = genTypeVariable

genBaseType :: Gen B.JBaseType
genBaseType = genericArbitraryU

genReferenceType :: Gen ReferenceType
genReferenceType = oneof
  [ RefClassType <$> genClassType
  , RefTypeVariable <$> genTypeVariable
  , RefArrayType <$> genType
  ]

genClassType :: Gen ClassType
genClassType = sized \case
  0 ->
    ClassType
      <$> elements ["some/inner/ClassType$inner", "some/base/ClassType"]
      <*> pure Nothing
      <*> listOf genTypeArgument
  n ->
    resize (n `div` 2)
      $   ClassType
      <$> pure "text"
      <*> (Just <$> genClassType)
      <*> listOf genTypeArgument

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
