{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module : Jvmhs.Data.Type
Copyright : (c) Christian Gram Kalhauge, 2019
License  : BSD3
Maintainer : kalhauge@cs.ucla.edu

This module describes the types of Jvmhs. The types here contain both the
signatures and anntotations of the jvm-binary package.


Notes: An annotation can be repeated multiple places. For example is an annotation
on a field both an annotation of its type and the field itself:
@
  public @A int field; -> A is on field and on type int.
@
-}
module Jvmhs.Data.Type (
  -- * Basic types
  JType (..),
  _JTRef,
  _JTBase,
  JBaseType (..),
  JRefType (..),
  _JTClass,
  _JTArray,
  ClassName (..),

  -- * Bigger types
  Type (..),
  _ReferenceType,
  _BaseType,
  ReferenceType (..),
  _RefClassType,
  _RefArrayType,
  _RefTypeVariable,

  -- ** ClassType
  ClassType (..),
  classTypeName,
  classTypeArguments,
  classTypeInner,
  extendClassType,
  classTypeFromName,
  classTypeFromNameAndTypeArgs,
  insertTypeArgument,
  innerClassTypeFromName,
  classNameFromType,

  -- ** ArrayType
  ArrayType (..),
  arrayType,

  -- ** ReturnType
  ReturnType (..),
  returnType,

  -- ** ThrowsType
  ThrowsType (..),
  _ThrowsClass,
  _ThrowsTypeVariable,
  classNameFromThrowsType,
  boundClassNameFromThrowsType,

  -- ** TypeParameter
  TypeParameter (..),
  typeParameterName,
  typeParameterClassBound,
  typeParameterInterfaceBound,

  -- ** TypeVariable
  TypeVariable (..),
  typeVariableBound,
  typeVariableName,
  unboundTypeVariable,
  TypeVariableName (..),
  unTypeVariableName,

  -- ** TypeArgument
  TypeArgument (..),
  _AnyTypeArg,
  _ExtendedTypeArg,
  _ImplementedTypeArg,
  _TypeArg,

  -- * IsSimple

  -- Tests if the types are simple types. This means that there
  -- are type variables or type applications used.
  typeIsSimple,
  referenceTypeIsSimple,
  classTypeIsSimple,
  arrayTypeIsSimple,
  annotatedIsSimple,
  returnTypeIsSimple,
  throwsTypeIsSimple,

  -- * Bind

  -- This enables bindings of classnames to variables
  bindType,
  bindReferenceType,
  bindReturnType,
  bindThrowsType,
  bindTypeVariable,

  -- * Annotations
  Annotations,
  AnnotationMap,
  Annotation (..),
  annotationValues,
  annotationIsRuntimeVisible,
  annotationType,
  getAnnotation,

  -- ** Accessors
  setTypeAnnotations,
  getTypeAnnotations,
  removeAnnotations,
  HasTypeAnnotations (..),

  -- ** Annotated
  Annotated (..),
  annotatedAnnotations,
  annotatedContent,
  withNoAnnotation,

  -- ** AnnotationValue
  AnnotationValue (..),
  _AByte,
  _AChar,
  _ADouble,
  _AFloat,
  _AInt,
  _ALong,
  _AShort,
  _ABoolean,
  _AString,
  _AEnum,
  _AClass,
  _AAnnotation,
  _AArray,

  -- * Helpers
  fromJType,
  toJType,
  toBoundJType,

  -- ** HasSimpleType
  HasSimpleType (..),

  -- ** Re-exports

  -- *** TypeAnnotation
  B.MethodTypeAnnotation (..),
  B.FieldTypeAnnotation (..),
  B.ClassTypeAnnotation (..),
  B.CodeTypeAnnotation (..),

  -- *** TypePath
  TypePath,
  B.TypePathItem (..),
  B.TypePathKind (..),

  -- *** EnumValue
  B.EnumValue (..),

  -- *** Values
  B.JValue (..),
)
where

-- base
import Data.Either
import Data.Foldable
import qualified Data.Kind as Kind
import Data.Maybe
import GHC.Generics (Generic)

-- deep-seq
import Control.DeepSeq

-- containers
import qualified Data.Map.Strict as Map
import Data.Sequence ((><))
import qualified Data.Sequence as Seq

-- unorderd-containers
import qualified Data.HashMap.Strict as HashMap

-- text
import qualified Data.Text as Text

-- lens
import Control.Lens hiding ((.=))

-- aeson
import Data.Aeson

-- jvm-binary
import qualified Language.JVM as B
import qualified Language.JVM.Attribute.Annotations as B
import Language.JVM.Type

-- cones
import Data.Cone.TH

{- | This is an annotated type paramater, modeled after `B.TypeParameter`.
 NOTE While the class bound and can technicaly be any reference type, it
 is not allowed in the Java language.
-}
data TypeParameter = TypeParameter
  { _typeParameterName :: !TypeVariableName
  , _typeParameterClassBound :: !(Maybe (Annotated ThrowsType))
  , _typeParameterInterfaceBound :: ![Annotated ThrowsType]
  }
  deriving (Show, Eq, Generic, NFData)

-- | A reference type can also be Annotated
data ReferenceType
  = RefClassType !ClassType
  | RefTypeVariable !TypeVariable
  | RefArrayType !ArrayType
  deriving (Show, Eq, Generic, NFData)

-- | A throw type can also be annotated
data ThrowsType
  = ThrowsClass !ClassType
  | ThrowsTypeVariable !TypeVariable
  deriving (Show, Eq, Generic, NFData)

{- | An 'ClassType' is interesting because it can represent inner classes
 in different ways.
-}
data ClassType = ClassType
  { _classTypeName :: !Text.Text
  , _classTypeInner :: !(Maybe (Annotated ClassType))
  , _classTypeArguments :: ![Annotated TypeArgument]
  }
  deriving (Show, Eq, Generic, NFData)

newtype ArrayType = ArrayType
  { _arrayType :: Annotated Type
  }
  deriving (Show, Eq, Generic)
  deriving newtype (NFData)

-- | A Return type is a type or void.
newtype ReturnType = ReturnType
  { _returnType :: Maybe Type
  }
  deriving (Show, Eq, Generic)
  deriving newtype (NFData)

data Type
  = ReferenceType !ReferenceType
  | BaseType !JBaseType
  deriving (Show, Eq, Generic, NFData)

{- | A Type argument is either any type @?@ (@*@), a bounded type
 @? extends Object@ (@+java/lang/Object@) or just a type
 @Object@ (@java/lang/Object@)
-}
data TypeArgument
  = AnyTypeArg
  | ExtendedTypeArg (Annotated ReferenceType)
  | ImplementedTypeArg (Annotated ReferenceType)
  | TypeArg !ReferenceType
  deriving (Show, Eq, Generic, NFData)

{- | A Type variable is a name and a bound. The bound is semi-artificial
 and is used to determine the type of the variable when flattened. It
 corresponds to the bound of the type parameter and should be changed if
 the typeparameter changes.
-}
data TypeVariable = TypeVariable
  { _typeVariableName :: !TypeVariableName
  , _typeVariableBound :: !ClassName
  }
  deriving (Show, Eq, Generic, NFData)

unboundTypeVariable :: TypeVariableName -> TypeVariable
unboundTypeVariable = flip TypeVariable "java/lang/Object"

newtype TypeVariableName = TypeVariableName
  { _unTypeVariableName :: Text.Text
  }
  deriving (Show, Eq, Generic)
  deriving newtype (NFData, ToJSON, FromJSON)

-- | An annotation is a map of names to values.
data Annotation = Annotation
  { _annotationType :: !ClassName
  -- ^ any field descriptor type is allowed here, but in practice only
  -- classes are used.
  , _annotationIsRuntimeVisible :: !Bool
  , _annotationValues :: !AnnotationMap
  }
  deriving (Show, Eq, Generic, NFData)

-- | An annotation map is a map of annotation types to annotation objects.
type AnnotationMap = HashMap.HashMap Text.Text AnnotationValue

-- | We collect annotations in a list.
type Annotations = [Annotation]

-- | A type can be annotated.
data Annotated a = Annotated
  { _annotatedContent :: !a
  , _annotatedAnnotations :: [Annotation]
  -- ^ It is assumed that the list does not contain dublicates.
  }
  deriving (Show, Eq, Generic, NFData, Functor, Foldable, Traversable)

-- | An annoation contains values which can be set using multiple types.
data AnnotationValue
  = AByte !B.VInteger
  | AChar !B.VInteger
  | ADouble !B.VDouble
  | AFloat !B.VFloat
  | AInt !B.VInteger
  | ALong !B.VLong
  | AShort !B.VInteger
  | ABoolean !B.VInteger
  | AString !B.VString
  | AEnum !(B.EnumValue B.High)
  | AClass !B.ReturnDescriptor
  | -- | Almost a complete annotation without the information about
    -- visibility this information is inheirited from
    AAnnotation !(ClassName, AnnotationMap)
  | AArray ![AnnotationValue]
  deriving (Show, Eq, Generic, NFData)

makeLenses ''ClassType
makeLenses ''ArrayType
makeLenses ''ReturnType
makeLenses ''TypeParameter
makeLenses ''TypeVariable
makeLenses ''TypeVariableName
makePrisms ''TypeArgument
makePrisms ''ThrowsType
makePrisms ''ReferenceType
makePrisms ''Type

makePrisms ''JType
makePrisms ''JRefType
makePrisms ''JBaseType

makeLenses ''Annotation
makePrisms ''AnnotationValue
makeLenses ''Annotated

{- | Get the name of a class type, this throws away all anotations and
 type signatures
-}
classNameFromType :: ClassType -> ClassName
classNameFromType ct =
  fromRight (error ("Unexpected behaviour, please report a bug: " <> show ct)) $
    B.textCls (Text.intercalate "$" $ nameOf ct)
 where
  nameOf t =
    t
      ^. classTypeName
      : foldMap
        nameOf
        (t ^? classTypeInner . _Just . annotatedContent)

classNameFromThrowsType :: ThrowsType -> Either TypeVariable ClassName
classNameFromThrowsType = \case
  ThrowsClass cn -> Right $ classNameFromType cn
  ThrowsTypeVariable tv -> Left tv

{- | Like 'classNameFromThrowsType' but tries to lookup any type variable
 in an environment first.
-}
boundClassNameFromThrowsType :: ThrowsType -> ClassName
boundClassNameFromThrowsType =
  either (view typeVariableBound) id . classNameFromThrowsType

-- | Extend a ClassType with an inner classType.
extendClassType :: Annotated ClassType -> ClassType -> ClassType
extendClassType ct =
  classTypeInner
    %~ ( Just . \case
          Just (Annotated x a) -> Annotated (extendClassType ct x) a
          Nothing -> ct
       )

-- | Creates a ClassType without any annotations and typesignatures
classTypeFromName :: ClassName -> ClassType
classTypeFromName = innerClassTypeFromName . classNameAsText

-- | Creates a ClassType without any annotations and typesignatures
classTypeFromNameAndTypeArgs
  :: ClassName -> [Annotated TypeArgument] -> ClassType
classTypeFromNameAndTypeArgs cn ta =
  insertTypeArgument ta . innerClassTypeFromName . classNameAsText $ cn

insertTypeArgument :: [Annotated TypeArgument] -> ClassType -> ClassType
insertTypeArgument ta ct@(ClassType _ x _) = case x of
  Nothing -> ct & classTypeArguments .~ ta
  Just x' ->
    ct
      & classTypeInner
        .~ Just
          (over annotatedContent (insertTypeArgument ta) x')

-- | Creates a ClassType without any annotations and typesignatures
innerClassTypeFromName :: Text.Text -> ClassType
innerClassTypeFromName = fromJust . go . Text.split (== '$')
 where
  go [] = Nothing
  go (a : as) = Just $ ClassType a (withNoAnnotation <$> go as) []

-- | Convert a Type to either A TypeVariable or a simple type.
toJType :: (TypeVariable -> ClassName) -> Type -> B.JType
toJType fn = \case
  ReferenceType rt -> B.JTRef (toJRefType fn rt)
  BaseType bt -> B.JTBase bt

{- | We can convert a JType to a 'Type' without any annotations or
 generics
-}
fromJType :: B.JType -> Type
fromJType = \case
  JTRef rt -> ReferenceType (fromJRefType rt)
  JTBase bt -> BaseType bt

toBoundJType :: Type -> B.JType
toBoundJType = toJType (view typeVariableBound)

{- | Create a 'ReferenceType' from a 'JRefType', without any annotations
 or generics.
-}
fromJRefType :: B.JRefType -> ReferenceType
fromJRefType = \case
  JTClass cn -> RefClassType $ classTypeFromName cn
  JTArray atp -> RefArrayType $ ArrayType (withNoAnnotation (fromJType atp))

{- | Convert a ReferenceType to either a 'TypeVariable' or a simple
 'B.JRefType'.
-}
toJRefType :: (TypeVariable -> ClassName) -> ReferenceType -> B.JRefType
toJRefType fn = \case
  RefClassType ct -> JTClass (classNameFromType ct)
  RefArrayType (ArrayType (Annotated atp _)) -> JTArray (toJType fn atp)
  RefTypeVariable tv -> JTClass (fn tv)

{- | Check if the type is a simple type. This means that there
 are no annotations
-}
typeIsSimple :: Type -> Bool
typeIsSimple = \case
  ReferenceType t -> referenceTypeIsSimple t
  BaseType _ -> True

referenceTypeIsSimple :: ReferenceType -> Bool
referenceTypeIsSimple = \case
  RefClassType ct -> classTypeIsSimple ct
  RefTypeVariable _ -> False
  RefArrayType atp -> arrayTypeIsSimple atp

annotatedIsSimple :: (a -> Bool) -> Annotated a -> Bool
annotatedIsSimple isSimple = view (annotatedContent . to isSimple)

classTypeIsSimple :: ClassType -> Bool
classTypeIsSimple =
  andOf
    ( (classTypeArguments . folded . like False)
        <> classTypeInner
          . folded
          . to
            (annotatedIsSimple classTypeIsSimple)
    )

-- | Check if an ArrayType is simple
arrayTypeIsSimple :: ArrayType -> Bool
arrayTypeIsSimple = view (arrayType . to (annotatedIsSimple typeIsSimple))

throwsTypeIsSimple :: ThrowsType -> Bool
throwsTypeIsSimple = \case
  ThrowsClass ct -> classTypeIsSimple ct
  ThrowsTypeVariable _ -> False

returnTypeIsSimple :: ReturnType -> Bool
returnTypeIsSimple = andOf (returnType . _Just . to typeIsSimple)

bindType :: JType -> Type -> Maybe Type
bindType = curry \case
  (JTRef t, ReferenceType rt) -> ReferenceType <$> bindReferenceType t rt
  (JTBase b, BaseType b') | b == b -> Just (BaseType b')
  _ -> Nothing

bindReferenceType :: JRefType -> ReferenceType -> Maybe ReferenceType
bindReferenceType = curry \case
  (JTClass cn, RefTypeVariable tv) ->
    Just . RefTypeVariable $ bindTypeVariable cn tv
  (JTArray t', RefArrayType t) -> RefArrayType <$> bindArrayType t' t
  (JTClass cn, RefClassType ct)
    | classNameFromType ct == cn ->
        Just (RefClassType ct)
  _ -> Nothing

bindArrayType :: JType -> ArrayType -> Maybe ArrayType
bindArrayType t' = arrayType . annotatedContent $ bindType t'

bindThrowsType :: ClassName -> ThrowsType -> Maybe ThrowsType
bindThrowsType cn = \case
  ThrowsTypeVariable tv -> Just . ThrowsTypeVariable $ bindTypeVariable cn tv
  ThrowsClass ct
    | classNameFromType ct == cn -> Just (ThrowsClass ct)
    | otherwise -> Nothing

bindReturnType :: Maybe JType -> ReturnType -> Maybe ReturnType
bindReturnType = curry \case
  (Just x, ReturnType (Just t)) -> ReturnType . Just <$> bindType x t
  (Nothing, ReturnType Nothing) -> Just $ ReturnType Nothing
  _ -> Nothing

bindTypeVariable :: ClassName -> TypeVariable -> TypeVariable
bindTypeVariable = set typeVariableBound

-- | Create an annotated value with no annotations
withNoAnnotation :: a -> Annotated a
withNoAnnotation a = Annotated a []

-- | Get an annotation with ClassName
getAnnotation :: ClassName -> Annotated a -> Maybe Annotation
getAnnotation cn =
  findOf (annotatedAnnotations . folded) (view $ annotationType . to (== cn))

-- | Redefining '@B.TypePath'
type TypePath = [B.TypePathItem]

-- | Many of the types have annotations, this have
class HasTypeAnnotations a where
  -- | A path points to a list of annotations or have an error
  typeAnnotations
    :: (ClassName -> Bool) -> IndexedTraversal' TypePath a [Annotation]

-- | Get a list of all annotations in the type
getTypeAnnotations
  :: HasTypeAnnotations a
  => (ClassName -> Bool)
  -- ^ Is the ClassName a static inner class
  -> a
  -> [(TypePath, Annotation)]
getTypeAnnotations isStatic = itoListOf (typeAnnotations isStatic <. traverse)

-- | Given a list of annotations set them in the type.
setTypeAnnotations
  :: HasTypeAnnotations a
  => (ClassName -> Bool)
  -> [(TypePath, Annotation)]
  -> a
  -> Either String a
setTypeAnnotations isStatic items a =
  let m =
        Map.map toList . Map.fromListWith (><) $ over _2 Seq.singleton <$> items
   in case Map.toList $
        m
          Map.\\ Map.fromList
            ( view (_1 . to (,())) <$> itoListOf (typeAnnotations isStatic) a
            ) of
        [] ->
          Right $
            a
              & typeAnnotations isStatic
                .@~ (\k -> reverse $ Map.findWithDefault [] k m)
        as -> Left $ "The type does not have the paths " <> show as

removeAnnotations :: HasTypeAnnotations a => (ClassName -> Bool) -> a -> a
removeAnnotations isStatic = set (typeAnnotations isStatic) []

typeAnnotationOfAnnotated
  :: IndexedTraversal' TypePath a [Annotation]
  -> IndexedTraversal' TypePath (Annotated a) [Annotation]
typeAnnotationOfAnnotated trav afb Annotated{..} = do
  _annotatedAnnotations' <-
    indexed
      afb
      ([] :: [B.TypePathItem])
      _annotatedAnnotations
  _annotatedContent' <- trav afb _annotatedContent
  pure $
    Annotated
      { _annotatedAnnotations = _annotatedAnnotations'
      , _annotatedContent = _annotatedContent'
      }

instance HasTypeAnnotations a => HasTypeAnnotations (Annotated a) where
  typeAnnotations isStatic =
    typeAnnotationOfAnnotated (typeAnnotations isStatic)

instance HasTypeAnnotations ArrayType where
  typeAnnotations isStatic =
    reindexed
      (B.TypePathItem B.TPathInArray 0 :)
      (arrayType . typeAnnotations isStatic)

instance HasTypeAnnotations Type where
  typeAnnotations isStatic afb = \case
    ReferenceType f -> ReferenceType <$> typeAnnotations isStatic afb f
    a -> pure a

instance HasTypeAnnotations ReferenceType where
  typeAnnotations isStatic afb = \case
    RefClassType f -> RefClassType <$> typeAnnotations isStatic afb f
    RefArrayType f -> RefArrayType <$> typeAnnotations isStatic afb f
    a -> pure a

instance HasTypeAnnotations ClassType where
  typeAnnotations isStatic _afb ct = go (_classTypeName ct) _afb ct
   where
    go :: Text.Text -> IndexedTraversal' TypePath ClassType [Annotation]
    go cn afb ClassType{..} = case _classTypeInner of
      Just ict
        | isStatic (B.unsafeTextCls icn) -> do
            nested <- (annotatedContent . go icn) afb ict
            pure $ ClassType _classTypeName (Just nested) _classTypeArguments
        | otherwise -> do
            nested <-
              reindexed
                (B.TypePathItem B.TPathInNested 0 :)
                (typeAnnotationOfAnnotated (go icn))
                afb
                ict

            typeargs <-
              icompose
                ( \(i :: Int) j ->
                    B.TypePathItem B.TPathTypeArgument (fromIntegral i) : j
                )
                itraversed
                (typeAnnotations isStatic)
                afb
                _classTypeArguments

            pure $ ClassType _classTypeName (Just nested) typeargs
       where
        icn = cn <> "$" <> ict ^. annotatedContent . classTypeName
      Nothing -> do
        typeargs <-
          icompose
            ( \(i :: Int) j ->
                B.TypePathItem B.TPathTypeArgument (fromIntegral i) : j
            )
            itraversed
            (typeAnnotations isStatic)
            afb
            _classTypeArguments

        pure $ ClassType _classTypeName Nothing typeargs

instance HasTypeAnnotations TypeArgument where
  typeAnnotations isStatic afb = \case
    ExtendedTypeArg t' ->
      ExtendedTypeArg
        <$> reindexed
          (B.TypePathItem B.TPathWildcard 0 :)
          (typeAnnotations isStatic)
          afb
          t'
    ImplementedTypeArg t' ->
      ImplementedTypeArg
        <$> reindexed
          (B.TypePathItem B.TPathWildcard 0 :)
          (typeAnnotations isStatic)
          afb
          t'
    TypeArg a -> TypeArg <$> (typeAnnotations isStatic) afb a
    AnyTypeArg -> pure AnyTypeArg

{- | Even though TypeParameter have type annotations they are not
 accessable through a path. Use the other functions instead.
-}
instance HasTypeAnnotations TypeParameter where
  typeAnnotations _ _ = pure

instance HasTypeAnnotations ThrowsType where
  typeAnnotations isStatic afb = \case
    ThrowsClass ct -> ThrowsClass <$> typeAnnotations isStatic afb ct
    a -> pure a

instance HasTypeAnnotations ReturnType where
  typeAnnotations isStatic = returnType . _Just . typeAnnotations isStatic

-- | These types here have simpler types
class HasSimpleType a where
  type SimpleType a :: Kind.Type

  -- Get the simpler type
  simpleType :: Getter a (SimpleType a)

instance HasSimpleType a => HasSimpleType (Annotated a) where
  type SimpleType (Annotated a) = SimpleType a
  simpleType = annotatedContent . simpleType
  {-# INLINE simpleType #-}

instance HasSimpleType Type where
  type SimpleType Type = JType
  simpleType = to toBoundJType
  {-# INLINE simpleType #-}

instance HasSimpleType ReturnType where
  type SimpleType ReturnType = Maybe JType
  simpleType = returnType . to (\(a :: Maybe Type) -> view simpleType <$> a)
  {-# INLINE simpleType #-}

instance HasSimpleType ArrayType where
  type SimpleType ArrayType = JType
  simpleType = arrayType . simpleType
  {-# INLINE simpleType #-}

instance HasSimpleType ThrowsType where
  type SimpleType ThrowsType = ClassName
  simpleType = to boundClassNameFromThrowsType
  {-# INLINE simpleType #-}

instance HasSimpleType ClassType where
  type SimpleType ClassType = ClassName
  simpleType = to classNameFromType
  {-# INLINE simpleType #-}

-- instance ToJSON Annotations where
--   toJSON Annotations {..} = object
--     ["visible" .= _visibleAnnotations, "invisible" .= _invisibleAnnotations]

-- instance ToJSON AnnotationValue where
--   toJSON = \case
--     AByte       a                 -> object ["byte" .= a]
--     AChar       a                 -> object ["char" .= chr (fromIntegral a)]
--     ADouble     a                 -> object ["double" .= a]
--     AFloat      a                 -> object ["float" .= a]
--     AInt        a                 -> object ["int" .= a]
--     ALong       a                 -> object ["long" .= a]
--     AShort      a                 -> object ["short" .= a]
--     ABoolean    a                 -> object ["boolean" .= (a /= 0)]
--     AString     a                 -> object ["string" .= a]
--     AEnum       (B.EnumValue a b) -> object ["enum" .= b, "enum_class" .= a]
--     AClass      a                 -> object ["class_info" .= a]
--     AAnnotation (name, a) -> object ["annotation" .= name, "values" .= a]
--     AArray      a                 -> object ["array" .= a]

$(makeDiagram ''Annotated)
$(makeDiagram ''ClassType)
$(makeDiagram ''Annotation)
$(makeDiagram ''TypeParameter)
$(makeDiagram ''B.ArrayType)
$(makeDiagram ''B.LocalType)
$(makeDiagram ''B.ArithmeticType)
$(makeDiagram ''B.SmallArithmeticType)
$(makeDiagram ''B.InRefType)
$(makeDiagram ''B.InClass)
$(makeDiagram ''MethodDescriptor)
$(makeDiagram ''JType)
$(makeDiagram ''JBaseType)
$(makeDiagram ''JRefType)
$(makeDiagram ''B.AbsVariableMethodId)
$(makeDiagram ''B.JValue)
