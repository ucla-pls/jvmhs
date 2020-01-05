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

import qualified Data.Vector                   as V

import qualified Data.Set                      as Set

import           Jvmhs.Format.ClassFile
import           Jvmhs.Format.Internal

import           Jvmhs.Data.Class
import           Jvmhs.Data.Code
import           Jvmhs.Data.Identifier

import           Jvmhs.Data.TypeSpec

spec :: Spec
spec = do
  spec_signature
  spec_annotationValueFormat
  spec_annotationsFormat

  spec_fieldType
  spec_fieldAttributes
  spec_field

  spec_methodSignature
  spec_methodAttributes
  spec_method

  spec_code

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

  describe "typeFormat" $ do
    test_formatter genType (flipDirection typeFormat)

spec_annotationValueFormat :: Spec
spec_annotationValueFormat = describe "annotationValueFormat" $ do
  test_formatter genAnnotationValue (flipDirection annotationValueFormat)

spec_annotationsFormat :: Spec
spec_annotationsFormat = describe "annotationsFormat" $ do
  test_formatter genAnnotations (flipDirection $ annotationsFormat True)

spec_fieldAttributes :: Spec
spec_fieldAttributes = describe "fieldAttributeFormat" $ do
  test_formatter genFieldAttributes (flipDirection fieldAttributesFormat)

 where
  genFieldAttributes = do
    value       <- pure (Just $ VInteger 0)
    annotations <- genAnnotations
    tpe         <- genAnnotated genType

    pure ((value, annotations), tpe)

spec_fieldType :: Spec
spec_fieldType = describe "fieldTypeFormat" $ do
  test_formatter (genAnnotated genType) (flipDirection fieldTypeFormat)

spec_field :: Spec
spec_field = describe "fieldFormat" $ do
  test_formatter genField (flipDirection fieldFormat)

 where

  genField = do
    _fieldName        <- pure "field"
    _fieldType        <- genAnnotated genType
    _fieldAccessFlags <- pure (Set.empty)
    _fieldValue       <- pure Nothing
    _fieldAnnotations <- genAnnotations
    pure Field { .. }

spec_code :: Spec
spec_code = describe "codeFormat" $ do
  test_formatter genCode (flipDirection codeFormat)

 where
  genCode = do
    _codeMaxStack       <- arbitrary
    _codeMaxLocals      <- arbitrary
    _codeExceptionTable <- listOf genExceptionHandler
    _codeByteCode       <- V.fromList <$> pure []
    _codeStackMap       <- pure Nothing
    pure Code { .. }

  genExceptionHandler =
    ExceptionHandler <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

spec_methodSignature :: Spec
spec_methodSignature = describe "methodSignatureFormat" $ do
  test_formatter genMethodAttributes (flipDirection methodSignatureFormat)

 where
  genMethodAttributes = do
    tp   <- listOf (genAnnotated genTypeParameter)
    parm <- listOf (genAnnotated genType)
    rt   <- genAnnotated genReturnType
    excp <- listOf (genAnnotated genThrowsType)
    pure (tp, parm, rt, excp)

spec_methodAttributes :: Spec
spec_methodAttributes = describe "methodAttributesFormat" $ do
  test_formatter genMethodAttributes (flipDirection methodAttributesFormat)

 where
  genMethodAttributes = do
    parm <- listOf (genAnnotated genType)
    rt   <- genAnnotated genReturnType
    tp   <- listOf (genAnnotated genTypeParameter)
    excp <- listOf (genAnnotated genThrowsType)
    code <- pure Nothing
    anno <- genAnnotations
    pure ((tp, parm, rt, excp), code, anno)

spec_method :: Spec
spec_method = describe "methodFormat" $ do
  test_formatter genMethod (flipDirection methodFormat)

 where

  genMethod = do
    _methodName           <- pure "method"
    _methodParameters     <- listOf (genAnnotated genType)
    _methodReturnType     <- genAnnotated genReturnType
    _methodTypeParameters <- listOf (genAnnotated genTypeParameter)
    _methodAccessFlags    <- pure (Set.empty)
    _methodCode           <- pure Nothing
    _methodExceptions     <- listOf (genAnnotated genThrowsType)
    _methodAnnotations    <- genAnnotations
    pure Method { .. }

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
