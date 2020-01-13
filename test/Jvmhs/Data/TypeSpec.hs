{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Jvmhs.Data.TypeSpec where

import           SpecHelper
import           Data.Either

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

  describe "annotations" $ do
    prop "getting and then setting should be id" $ \(a :: Type) -> do
      setTypeAnnotations (getTypeAnnotations a) a === Right a

    it "getting should work on this" $ do
      getTypeAnnotations
          (ReferenceType . RefClassType $ ClassType
            "Annotations"
            (Just (Annotated (ClassType "Annotated" Nothing []) []))
            [ Annotated
                (TypeArg . RefArrayType . ArrayType $ Annotated
                  (BaseType "I")
                  [ Annotation "Annotations$TestType"
                               False
                               (HashMap.fromList [("value", AInt 0)])
                  ]
                )
                []
            ]
          )
        `shouldBe` [ ( [ TypePathItem TPathTypeArgument 0
                       , TypePathItem TPathInArray      0
                       ]
                     , Annotation "Annotations$TestType"
                                  False
                                  (HashMap.fromList [("value", AInt 0)])
                     )
                   ]

    it "should work on this example" $ do
      setTypeAnnotations
          [ ( [TypePathItem TPathTypeArgument 0, TypePathItem TPathInArray 0]
            , Annotation "Annotations$TestType"
                         False
                         (HashMap.fromList [("value", AInt 0)])
            )
          ]
          (ReferenceType . RefClassType $ ClassType
            "Annotations"
            (Just (Annotated (ClassType "Annotated" Nothing []) []))
            [ Annotated
                (TypeArg . RefArrayType . ArrayType $ Annotated (BaseType "I")
                                                                []
                )
                []
            ]
          )
        `shouldSatisfy` isRight

-- * Generators

genBaseType :: Gen JBaseType
genBaseType = genericArbitraryU

genReferenceType :: [TypeParameter] -> Gen ReferenceType
genReferenceType tp =
  oneof
    $  [RefClassType <$> genClassType tp, RefArrayType <$> genArrayType tp]
    ++ [ RefTypeVariable <$> genTypeVariable tp | not (null tp) ]

genReturnType :: [TypeParameter] -> Gen ReturnType
genReturnType tp = ReturnType <$> liftArbitrary (genType tp)

genThrowsType :: [TypeParameter] -> Gen ThrowsType
genThrowsType tp =
  scale (`div` 2)
    .  oneof
    $  [ThrowsClass <$> genClassType tp]
    ++ [ ThrowsTypeVariable <$> genTypeVariable tp | not (null tp) ]

genClassType :: [TypeParameter] -> Gen ClassType
genClassType tp =
  scale (`div` 2)
    $   ClassType
    <$> elements ["A", "pkg/A", "a/B"]
    <*> genInnerClassType
    <*> listOf (genAnnotated (genTypeArgument tp))
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
        <*> listOf (genAnnotated (genTypeArgument tp))

genAnnotated :: Gen a -> Gen (Annotated a)
genAnnotated f = scale (`div` 2) $ Annotated <$> f <*> genAnnotations

genTypeArgument :: [TypeParameter] -> Gen TypeArgument
genTypeArgument tp = scale (`div` 2) $ oneof
  [ pure AnyTypeArg
  , TypeArg <$> genReferenceType tp
  , ExtendedTypeArg <$> genAnnotated (genReferenceType tp)
  , ImplementedTypeArg <$> genAnnotated (genReferenceType tp)
  ]

genTypeParameter :: [TypeParameter] -> Gen TypeParameter
genTypeParameter tp =
  scale (`div` 2)
    $   TypeParameter
    <$> (TypeVariableName <$> elements ["A", "B", "T"])
    <*> liftArbitrary (genAnnotated $ genThrowsType tp)
    <*> listOf (genAnnotated $ genThrowsType tp)

genType :: [TypeParameter] -> Gen Type
genType tp =
  oneof [ReferenceType <$> genReferenceType tp, BaseType <$> genBaseType]

genArrayType :: [TypeParameter] -> Gen ArrayType
genArrayType tp = ArrayType <$> genAnnotated (genType tp)

genTypeVariable :: [TypeParameter] -> Gen TypeVariable
genTypeVariable tp =
  TypeVariable <$> elements (map (view typeParameterName) tp) <*> arbitrary

-- instance Arbitrary TypeVariable where
--   arbitrary = genTypeVariable

instance Arbitrary ClassName where
  arbitrary =
    elements ["JustClass", "more/ClassName", "with/inner/Class$className"]

instance Arbitrary JRefType where
  arbitrary = scale (`div` 2) genericArbitraryU

instance Arbitrary Type where
  arbitrary = listOf (genTypeParameter []) >>= genType

instance Arbitrary JBaseType where
  arbitrary = genericArbitraryU

instance Arbitrary JType where
  arbitrary = genericArbitraryU

instance Arbitrary B.FieldDescriptor where
  arbitrary = elements ["I", "Ljava/lang/String;"]

instance Arbitrary B.MethodDescriptor where
  arbitrary = elements ["(I)V", "()Ljava/lang/String;", "(II)I"]

instance Arbitrary B.AbsFieldId where
  arbitrary = B.AbsFieldId <$> genericArbitraryU

instance Arbitrary B.AbsMethodId where
  arbitrary = B.AbsMethodId <$> genericArbitraryU

instance Arbitrary B.MethodHandleFieldKind where
  arbitrary = genericArbitraryU

instance Arbitrary B.ReturnDescriptor where
  arbitrary = genericArbitraryU

instance Arbitrary B.FieldId where
  arbitrary =
    B.FieldId <$> (B.NameAndType <$> elements ["f1", "f2"] <*> arbitrary)

instance Arbitrary B.MethodId where
  arbitrary =
    B.MethodId <$> (B.NameAndType <$> elements ["m1", "m2"] <*> arbitrary)

instance Arbitrary (B.InRefType B.MethodId) where
  arbitrary = genericArbitraryU

instance Arbitrary B.AbsVariableMethodId where
  arbitrary = genericArbitraryU

instance Arbitrary B.AbsInterfaceMethodId where
  arbitrary = genericArbitraryU


genJValue :: Gen JValue
genJValue = oneof
  [ VInteger <$> arbitrary
  , VLong <$> arbitrary
  , VFloat <$> arbitrary
  , VDouble <$> arbitrary
  , VString <$> elements ["", "string", "with spaces"]
  , VMethodType <$> arbitrary
  , VMethodHandle <$> genMethodHandle
  ]


genMethodHandle :: Gen (B.MethodHandle B.High)
genMethodHandle = scale (`div` 2) $ oneof
  [ B.MHField <$> genericArbitraryU
  , B.MHMethod <$> genericArbitraryU
  , B.MHInterface <$> genericArbitraryU
  ]

instance Arbitrary (B.MethodHandle B.High) where
  arbitrary = genMethodHandle


genAnnotation :: Gen Annotation
genAnnotation =
  scale (`div` 2)
    $   Annotation
    <$> elements ["an/annotations/Ano"]
    <*> arbitrary
    <*> genAnnotationMap

genAnnotationWithVisibility :: Bool -> Gen Annotation
genAnnotationWithVisibility visible =
  scale (`div` 2)
    $   Annotation
    <$> elements ["an/annotations/Ano"]
    <*> pure visible
    <*> genAnnotationMap

genAnnotationMap :: Gen AnnotationMap
genAnnotationMap = HashMap.fromList
  <$> listOf ((,) <$> elements ["value", "item", "x"] <*> genAnnotationValue)

genAnnotations :: Gen Annotations
genAnnotations =
  scale (`div` 2)
    $   (++)
    <$> listOf (genAnnotationWithVisibility True)
    <*> listOf (genAnnotationWithVisibility False)

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
