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

import qualified Data.Set                      as Set

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


instance Arbitrary MethodDescriptor where
  arbitrary = genericArbitraryU
