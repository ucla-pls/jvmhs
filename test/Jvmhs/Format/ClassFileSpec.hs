{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Jvmhs.Format.ClassFileSpec where

import           SpecHelper

import           Text.Nicify

-- base
import           Control.Monad
import           Data.Either
import qualified Data.List                     as List
import           Text.Printf

-- text
import qualified Data.Text                     as Text

-- HUnit
import           Test.HUnit

-- quickcheck
import           Test.QuickCheck

-- directory
import           System.Directory

-- bytestring
import qualified Data.ByteString.Lazy          as BL

import qualified Data.Vector                   as V

import qualified Data.Set                      as Set

import qualified Language.JVM                  as B
import qualified Language.JVM.Attribute.Code   as B
import qualified Language.JVM.Attribute.Annotations
                                               as B

import           Jvmhs.Format.ClassFile
import           Jvmhs.Format.Internal         as Format

import           Jvmhs.Data.Class
import           Jvmhs.Data.Type
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

  spec_code

  spec_annotateMethodTypes
  spec_parameterAnnotations
  spec_methodAttributes
  spec_method

  spec_class

  spec_testclasses

spec_signature :: Spec
spec_signature = describe "fromSignature conversions" $ do
  describe "typeFromJTypeFormat" $ do
    test_formatter (genType []) (flipDirection typeFromJTypeFormat)

  describe "classTypeFormat" $ do
    test_formatter (genClassType []) (flipDirection classTypeFormat)
    test_isomorphism (removeAnnotations)
                     (genClassType [])
                     (flipDirection classTypeFormat)

  describe "referenceTypeFromSignature" $ do
    test_formatter (genReferenceType [])
                   (flipDirection referenceTypeFromSignature)

  describe "typeFormat" $ do
    test_formatter (genType []) (flipDirection typeFormat)

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
    tpe         <- genAnnotated (genType [])

    pure ((value, annotations), tpe)

spec_fieldType :: Spec
spec_fieldType = describe "fieldTypeFormat" $ do
  test_formatter (genAnnotated (genType [])) (flipDirection fieldTypeFormat)

spec_field :: Spec
spec_field = describe "fieldFormat" $ do
  test_formatter genField (flipDirection fieldFormat)

 where
  genField = do
    _fieldName        <- pure "field"
    _fieldType        <- genAnnotated (genType [])
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
    _codeStackMap       <- liftArbitrary genStackMap
    pure Code { .. }

  genExceptionHandler =
    ExceptionHandler <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

  genStackMap =
    StackMapTable <$> listOf (StackMapFrame <$> arbitrary <*> genericArbitraryU)

instance Arbitrary a => Arbitrary (B.SizedList w a) where
  arbitrary = B.SizedList <$> listOf arbitrary

instance Arbitrary (VerificationTypeInfo B.High) where
  arbitrary = genericArbitraryU

-- spec_methodSignature :: Spec
-- spec_methodSignature = describe "methodSignatureFormat" $ do
--   test_formatter genMethodAttributes (flipDirection methodSignatureFormat)

--  where
--   genMethodAttributes = do
--     tp   <- listOf (genAnnotated genTypeParameter)
--     parm <- listOf genParameter
--     rt   <- genAnnotated genReturnType
--     excp <- listOf (genAnnotated genThrowsType)
--     pure (tp, parm, rt, excp)

spec_parameterAnnotations :: Spec
spec_parameterAnnotations = describe "parameterAnnotationsFormat" $ do
  test_formatter (listOf (genParameter []))
                 (flipDirection parameterAnnotationsFormat)

spec_annotateMethodTypes :: Spec
spec_annotateMethodTypes = describe "annotateMethodTypesFormat" $ do
  test_formatter (genMethodTypes) (flipDirection annotateMethodTypesFormat)
 where
  genMethodTypes = do
    tts <- listOf (genTypeParameter [])
    tpa <- listOf (genAnnotated $ genTypeParameter tts)
    pa  <- listOf (genParameter tts)
    ra  <- genAnnotated (genReturnType tts)
    tta <- listOf (genAnnotated (genThrowsType tts))
    pure (tpa, pa, ra, tta)

spec_methodAttributes :: Spec
spec_methodAttributes = describe "methodAttributesFormat" $ do
  test_formatter genMethodAttributes (flipDirection methodAttributesFormat)

 where
  clsparam            = [TypeParameter (TypeVariableName "V") Nothing []]

  genMethodAttributes = do
    tp <- listOf (genAnnotated (genTypeParameter clsparam))

    let tparams = (map (view annotatedContent) tp ++ clsparam)

    parm <- listOf (genParameter tparams)
    rt   <- genAnnotated (genReturnType tparams)
    excp <- listOf (genAnnotated (genThrowsType tparams))
    code <- pure Nothing
    anno <- genAnnotations
    pure ((tp, parm, rt, excp), code, anno)

spec_method :: Spec
spec_method = describe "methodFormat" $ do
  test_formatter genMethod (flipDirection methodFormat)

 where
  clsparam  = [TypeParameter (TypeVariableName "V") Nothing []]

  genMethod = do
    _methodTypeParameters <- listOf (genAnnotated $ genTypeParameter clsparam)

    let params = map (view annotatedContent) _methodTypeParameters ++ clsparam

    _methodName        <- pure "method"
    _methodParameters  <- listOf (genParameter params)
    _methodReturnType  <- genAnnotated (genReturnType params)
    _methodAccessFlags <- pure (Set.empty)
    _methodCode        <- pure Nothing
    _methodExceptions  <- listOf (genAnnotated $ genThrowsType params)
    _methodAnnotations <- pure []
    pure Method { .. }

spec_class :: Spec
spec_class = do
  describe "classFormat" $ do
    test_formatter genClass (flipDirection classFormat)

 where
  genClass = do
    let _className'       = "some/ObjectType" :: ClassName
    let _classAccessFlags = Set.empty
    _classTypeParameters <- listOf . genAnnotated $ genTypeParameter []
    let params = map (view annotatedContent) _classTypeParameters
    _classSuper      <- liftArbitrary . genAnnotated $ genClassType params
    _classInterfaces <- liftArbitrary . genAnnotated $ genClassType params
    let _classFields  = []
    let _classMethods = []
    _classVersion          <- liftArbitrary (pure (52, 0))
    _classBootstrapMethods <- listOf genBootstrapMethod
    _classEnclosingMethod  <- arbitrary
    _classInnerClasses     <- listOf genInnerClass
    _classAnnotations      <- pure []
    pure Class { .. }

  genInnerClass =
    InnerClass
      <$> arbitrary
      <*> arbitrary
      <*> liftArbitrary (elements ["InnerClass"])
      <*> pure Set.empty

spec_testclasses :: Spec
spec_testclasses = do
  describe
    "classes"
    do
      classes <- runIO $ do
        doesDirectoryExist "test/data/classes" >>= \case
          True -> do
            listDirectory "test/data/classes" >>= mapM
              (\fn -> (fn, ) <$> BL.readFile ("test/data/classes/" ++ fn))
          False -> return []

      forM_ classes $ \(fn, c) -> testClass fn c

  describe
    "extras"
    do
      classes <- runIO $ do
        doesDirectoryExist "test/data/extras" >>= \case
          True -> do
            listDirectory "test/data/extras" >>= mapM
              (\fn -> (fn, ) <$> BL.readFile ("test/data/extras/" ++ fn))
          False -> return []

      forM_ classes $ \(fn, c) -> testClass fn c

 where
  testClass fn c = describe fn $ do
    let cf = B.readClassFile c
    it "should read the classfile" $ do
      cf `shouldSatisfy` isRight

    case cf of
      Right r -> do
        let
          cleaned =
            r
              &  cMethods
              .  traverse
              .  mAttributesL
              .  lens B.maCode (\a x -> a { B.maCode = x })
              .  traverse
              %~ set
                   ( lens B.codeByteCode (\a x -> a { B.codeByteCode = x })
                   . lens B.byteCodeSize (\a x -> a { B.byteCodeSize = x })
                   )
                   0
              .  set
                   ( lens B.codeAttributes (\a x -> a { B.codeAttributes = x })
                   . lens
                       B.caLineNumberTable
                       (\a x -> a { B.caLineNumberTable = x })
                   )
                   []

        describe "fields" $ forM_ (B.cFields' cleaned) $ \f ->
          it
              (printf
                "can handle %s"
                (Text.unpack $ B.serialize (B.fName f <:> B.fDescriptor f))
              )
            $ do
                test_formatterOn f fieldFormat
                test_limitedIsomorphism id f fieldFormat

        describe "methods" $ forM_ (B.cMethods' cleaned) $ \m ->
          it
              (printf
                "can handle %s"
                (Text.unpack $ B.serialize (B.mName m <:> B.mDescriptor m))
              )
            $ do
                test_formatterOn m methodFormat
                test_limitedIsomorphism
                  (  mAttributesL
                  %~ ( over maVisibleTypeAnnotationsL   (map List.sort)
                     . over maInvisibleTypeAnnotationsL (map List.sort)
                     )
                  )
                  m
                  methodFormat

        it "everything" $ do
          test_formatterOn cleaned classFormat
          test_limitedIsomorphism (set cMethods [] . set cFields [])
                                  cleaned
                                  classFormat
      Left _ -> return ()


  cMethods :: Lens' (B.ClassFile B.High) [B.Method B.High]
  cMethods = lens B.cMethods' (\a x -> a { B.cMethods' = x }) . coerced

  cFields :: Lens' (B.ClassFile B.High) [B.Field B.High]
  cFields = lens B.cFields' (\a x -> a { B.cFields' = x }) . coerced

  mAttributesL :: Lens' (B.Method B.High) (B.MethodAttributes B.High)
  mAttributesL = lens B.mAttributes (\a x -> a { B.mAttributes = x })

  maInvisibleTypeAnnotationsL
    :: Lens'
         (B.MethodAttributes B.High)
         [[B.TypeAnnotation B.MethodTypeAnnotation B.High]]
  maInvisibleTypeAnnotationsL =
    lens B.maInvisibleTypeAnnotations
         (\a x -> a { B.maInvisibleTypeAnnotations = x })
      . coerced

  maVisibleTypeAnnotationsL
    :: Lens'
         (B.MethodAttributes B.High)
         [[B.TypeAnnotation B.MethodTypeAnnotation B.High]]
  maVisibleTypeAnnotationsL =
    lens B.maVisibleTypeAnnotations
         (\a x -> a { B.maVisibleTypeAnnotations = x })
      . coerced


genParameter :: [TypeParameter] -> Gen Parameter
genParameter tps =
  scale (`div` 2)
    $   Parameter
    <$> pure Nothing
    -- TODO: Not yet supported.
    <*> pure True
    <*> genAnnotated (genType tps)
    <*> genAnnotations

genBootstrapMethod :: Gen BootstrapMethod
genBootstrapMethod = BootstrapMethod <$> genMethodHandle <*> listOf genJValue

test_formatterOn :: (Eq a, Eq b, Show b, Show a) => a -> Formatter a b -> IO ()
test_formatterOn a PartIso { there, back } =
  let b  = there a
      a' = b >>= back
  in  do
        (there =<< a') `shouldBe` b

test_limitedIsomorphism
  :: (Eq a, Eq b, Show b, Show a) => (a -> a) -> a -> Formatter a b -> IO ()
test_limitedIsomorphism limit a PartIso { there, back } =
  let b  = there a
      a' = b >>= back
  in  do
        fmap limit a' `shouldBe` fmap limit (Format.Success a)

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
          $ do
              (there =<< a') `shouldBe` there a
              case b of
                Format.Failure x -> assertString (unlines x)
                _                -> return ()

test_isomorphism
  :: (Eq a, Eq b, Show b, Show a) => (a -> a) -> Gen a -> Formatter a b -> Spec
test_isomorphism limit gen PartIso { there, back } =
  prop "should be an isomorphism" . forAll gen $ \a ->
    let b  = there a
        a' = b >>= back
    in  counterexample
          (nicify (show b))
          (fmap limit a' `shouldBe` fmap limit (Format.Success a))
