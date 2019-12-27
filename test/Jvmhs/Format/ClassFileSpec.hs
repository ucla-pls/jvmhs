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

import           Text.Nicify

import           Test.QuickCheck

import           Control.Monad

import qualified Data.Set                      as Set


-- unordered-containers
import qualified Data.HashMap.Strict           as HashMap

import qualified Language.JVM                  as B
import qualified Language.JVM.Attribute.Annotations
                                               as B

import           Jvmhs.Format.ClassFile
import           Jvmhs.Data.Class
import           Jvmhs.Data.Identifier
import           Jvmhs.Data.Annotation

import           Jvmhs.Data.TypeSpec

spec :: Spec
spec = do
  spec_signature
  spec_annotationValueFormat
  spec_annotationsFormat
  spec_fieldType
  spec_fieldAttributes
  spec_field

spec_signature :: Spec
spec_signature = describe "fromSignature conversions" $ do
  describe "typeFromJTypeFormat" $ do
    test_formatter
      genType
      (flipDirection $ typeFromJTypeFormat (const "Ljava/lang/Object;"))

  describe "classTypeFromSignature" $ do
    test_formatter genClassType (flipDirection classTypeFromSignature)

  describe "referenceTypeFromSignature" $ do
    test_formatter genReferenceType (flipDirection referenceTypeFromSignature)

  describe "typeFromSignature" $ do
    test_formatter genType (flipDirection typeFromSignature)

spec_annotationValueFormat :: Spec
spec_annotationValueFormat = describe "annotationValueFormat" $ do
  test_formatter genAnnotationValue (flipDirection annotationValueFormat)

spec_annotationsFormat :: Spec
spec_annotationsFormat = describe "annotationsFormat" $ do
  test_formatter genAnnotations (flipDirection annotationsFormat)

spec_fieldAttributes :: Spec
spec_fieldAttributes = describe "fieldAttributeFormat" $ do
  test_formatter genFieldAttributes (flipDirection fieldAttributesFormat)

 where
  genFieldAttributes = do
    value       <- pure (Just $ VInteger 0)
    annotations <- genAnnotations
    tpe         <- genType

    pure ((value, annotations), tpe)

spec_fieldType :: Spec
spec_fieldType = describe "fieldTypeFormat" $ do
  test_formatter genType (flipDirection fieldTypeFormat)

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
    let b  = there a
        a' = b >>= back
    in  counterexample
          (  nicify (show a)
          ++ "\n--- there --> \n"
          ++ nicify (show b)
          ++ "\n <-- back  --- \n"
          ++ nicify (show a')
          )
          ((there =<< a') `shouldBe` there a)


instance Arbitrary B.ReturnDescriptor where
  arbitrary = genericArbitraryU

instance Arbitrary FieldDescriptor where
  arbitrary = genericArbitraryU

instance Arbitrary MethodDescriptor where
  arbitrary = genericArbitraryU

genAnnotations :: Gen Annotations
genAnnotations = Annotations <$> genAnnotationMap <*> genAnnotationMap
 where
  genAnnotationMap = HashMap.fromList
    <$> listOf (liftM2 (,) (elements ["a", "b", "c"]) genAnnotation)

genAnnotation :: Gen Annotation
genAnnotation = HashMap.fromList
  <$> listOf (liftM2 (,) (elements ["a1", "b1", "c1"]) genAnnotationValue)

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
  , AString <$> elements ["a-string", "a-longer-string"]
  , AEnum <$> genEnumValue
  , AClass <$> arbitrary
  , AAnnotation <$> liftM2 (,) (elements ["nested"]) genAnnotation
  , AArray <$> listOf genAnnotationValue
  ]
  where genEnumValue = B.EnumValue <$> arbitrary <*> pure "enum-const-name"
