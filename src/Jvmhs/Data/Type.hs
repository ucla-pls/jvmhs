{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-|
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
module Jvmhs.Data.Type
  (
  -- * Basic types 
    JType(..)
  , _JTRef
  , _JTBase
  , JBaseType(..)
  , JRefType(..)
  , _JTClass
  , _JTArray
  , ClassName(..)

  -- * Bigger types
  , Type(..)
  , _ReferenceType
  , _BaseType
  , ReferenceType(..)
  , _RefClassType
  , _RefArrayType
  , _RefTypeVariable

  -- ** ClassType
  , ClassType(..)
  , classTypeName
  , classTypeArguments
  , classTypeInner
  , extendClassType
  , classTypeFromName
  , innerClassTypeFromName
  , classNameFromType

  -- ** ArrayType
  , ArrayType(..)
  , arrayType

  -- ** Others
  , TypeParameter(..)
  , ThrowsSignature(..)
  , TypeVariable(..)
  , TypeArgument(..)

  -- * Annotations
  -- This module uses annotations from the annotations module, we have 
  -- re-exported Annotated here for convinence.
  , Annotated(..)
  , B.TypePath
  , B.TypePathItem(..)
  , setTypeAnnotations
  , getTypeAnnotations

  -- * Helpers
  , fromJType
  , toJType

  -- * Re-exports
  , B.JValue(..)
  )
where

-- base
import           Data.Either
import           Data.Maybe
import           Data.List.NonEmpty             ( NonEmpty(..)
                                                , nonEmpty
                                                )
import           GHC.Generics                   ( Generic )

-- deep-seq
import           Control.DeepSeq

-- text
import qualified Data.Text                     as Text

-- lens
import           Control.Lens            hiding ( (.=) )

-- jvm-binary
import qualified Language.JVM                  as B
import           Language.JVM.Type
import qualified Language.JVM.Attribute.Annotations
                                               as B

-- jvmhs
import           Jvmhs.Data.Annotation

-- | This is an annotated type paramater, modeled after `B.TypeParameter`.
data TypeParameter = TypeParameter
  { _typeIdentifier :: ! Text.Text
  , _typeClassBound     :: ! (Maybe ReferenceType)
  , _typeInterfaceBound :: ! [ReferenceType]
  } deriving (Show, Eq, Generic, NFData)

-- | A reference type can also be Annotated
data ReferenceType
  = RefClassType !ClassType
  | RefTypeVariable !TypeVariable
  | RefArrayType !ArrayType
  deriving (Show, Eq, Generic, NFData)

-- | A throw signature can also be annotated
data ThrowsSignature
  = ThrowsClass !ClassType
  | ThrowsTypeVariable !TypeVariable
  deriving (Show, Eq, Generic, NFData)

-- | An 'ClassType' is interesting because it can represent inner classes
-- in different ways.
data ClassType = ClassType
  { _classTypeName :: ! Text.Text
  , _classTypeInner :: ! (Maybe (Annotated ClassType))
  , _classTypeArguments :: [ Annotated TypeArgument ]
  } deriving (Show, Eq, Generic, NFData)

newtype ArrayType = ArrayType
  { _arrayType :: Annotated Type
  } deriving (Show, Eq, Generic, NFData)

data Type
  = ReferenceType !ReferenceType
  | BaseType !JBaseType
  deriving (Show, Eq, Generic, NFData)

-- | A Type argument is either any type @?@ (@*@), a bounded type 
-- @? extends Object@ (@+java/lang/Object@) or just a type 
-- @Object@ (@java/lang/Object@)
data TypeArgument
  = AnyTypeArg
  | ExtendedTypeArg (Annotated ReferenceType)
  | ImplementedTypeArg (Annotated ReferenceType)
  | TypeArg !ReferenceType
  deriving (Show, Eq, Generic, NFData)

newtype TypeVariable = TypeVariable
  { _typeVarName :: Text.Text
  } deriving (Show, Eq, Generic, NFData)

makeLenses ''ClassType
makeLenses ''ArrayType
makePrisms ''ThrowsSignature
makePrisms ''ReferenceType
makePrisms ''Type

makePrisms ''JType
makePrisms ''JRefType
makePrisms ''JBaseType

-- | Get the name of a class type, this throws away all anotations and 
-- type signatures
classNameFromType :: ClassType -> ClassName
classNameFromType ct =
  fromRight (error "Unexpected behaviour, please report a bug")
    $ B.textCls (Text.intercalate "$" $ nameOf ct)
 where
  nameOf t = t ^. classTypeName : maybe
    []
    nameOf
    (t ^? classTypeInner . _Just . annotatedContent)

-- | Extend a ClassType with an inner classType.
extendClassType :: ClassType -> ClassType -> ClassType
extendClassType ct =
  classTypeInner
    %~ (Just . \case
         Just (Annotated x a) -> Annotated (extendClassType ct x) a
         Nothing              -> withNoAnnotation ct
       )

-- | Creates a ClassType without any annotations and typesignatures
classTypeFromName :: ClassName -> ClassType
classTypeFromName = innerClassTypeFromName . classNameAsText

-- | Creates a ClassType without any annotations and typesignatures
innerClassTypeFromName :: Text.Text -> ClassType
innerClassTypeFromName = fromJust . go . Text.split (== '$')
 where
  go []       = Nothing
  go (a : as) = Just $ ClassType a (withNoAnnotation <$> go as) []

-- | Convert a Type to either A TypeVariable or a simple type.
toJType :: Type -> Either TypeVariable B.JType
toJType = \case
  ReferenceType rt -> B.JTRef <$> toJRefType rt
  BaseType      bt -> Right $ B.JTBase bt

-- | We can convert a JType to a 'Type' without any annotations or 
-- generics
fromJType :: B.JType -> Type
fromJType = \case
  JTRef  rt -> ReferenceType (fromJRefType rt)
  JTBase bt -> BaseType bt

-- | Create a 'ReferenceType' from a 'JRefType', without any annotations 
-- or generics.
fromJRefType :: B.JRefType -> ReferenceType
fromJRefType = \case
  JTClass cn  -> RefClassType $ classTypeFromName cn
  JTArray atp -> RefArrayType $ ArrayType (withNoAnnotation (fromJType atp))

-- | Convert a ReferenceType to either a 'TypeVariable' or a simple
-- 'B.JRefType'.
toJRefType :: ReferenceType -> Either TypeVariable B.JRefType
toJRefType = \case
  RefClassType ct -> Right $ JTClass (classNameFromType ct)
  RefArrayType (ArrayType (Annotated atp _)) -> JTArray <$> toJType atp
  RefTypeVariable tv -> Left tv

setTypeAnnotations
  :: [(B.TypePath, Annotation)] -> Type -> Either String (Annotated Type)
setTypeAnnotations path t = foldr
  (\(tp, a) t' -> addAnnotations addTypeAnnotations (B.unSizedList tp) a =<< t')
  (Right $ withNoAnnotation t)
  path

badPath :: B.TypePathItem -> String -> Either String a
badPath item str = Left $ "Could not follow " ++ show item ++ " in " ++ str


lensAct :: Monad m => Lens s s a a -> (a -> m a) -> s -> m s
lensAct ln fn s = fn (s ^. ln) <&> \b -> set ln b s

updateOrFail
  :: Traversal' s a
  -> (a -> Either String a)
  -> String
  -> s
  -> (Either String s)
updateOrFail ln fn err s = case s ^? ln of
  Nothing -> Left err
  Just a  -> fn a <&> \b -> set ln b s


addAnnotations
  :: (NonEmpty B.TypePathItem -> Annotation -> a -> Either String a)
  -> [B.TypePathItem]
  -> Annotation
  -> Annotated a
  -> Either String (Annotated a)
addAnnotations fn path a t = case nonEmpty path of
  Nothing    -> Right $ t & annotatedAnnotations %~ (a :)
  Just npath -> lensAct annotatedContent (fn npath a) t

addArrayTypeAnnotations
  :: NonEmpty B.TypePathItem
  -> Annotation
  -> ArrayType
  -> Either String ArrayType
addArrayTypeAnnotations (i :| rest) a t = case i of
  B.TypePathItem B.TPathInArray 0 ->
    lensAct arrayType (addAnnotations addTypeAnnotations rest a) t
  _ -> badPath i "ArrayType"

addTypeAnnotations
  :: NonEmpty B.TypePathItem -> Annotation -> Type -> Either String Type
addTypeAnnotations path a = \case
  ReferenceType rt -> ReferenceType <$> addReferenceTypeAnnotations path a rt
  BaseType _ ->
    Left
      $  "Cannot annotate a base type deeper, but more path is left: "
      ++ show path

addReferenceTypeAnnotations
  :: NonEmpty B.TypePathItem
  -> Annotation
  -> ReferenceType
  -> Either String ReferenceType
addReferenceTypeAnnotations path a = \case
  RefClassType ct  -> RefClassType <$> addClassTypeAnnotations path a ct
  RefArrayType rat -> RefArrayType <$> addArrayTypeAnnotations path a rat
  RefTypeVariable _ ->
    Left
      $  "Cannot annotate a type variable deeper, but more path is left: "
      ++ show path

addClassTypeAnnotations
  :: NonEmpty B.TypePathItem
  -> Annotation
  -> ClassType
  -> Either String ClassType
addClassTypeAnnotations (tpi@(B.TypePathItem k i) :| rest) a t = case k of
  B.TPathInNested -> updateOrFail
    (classTypeInner . _Just)
    (addAnnotations addClassTypeAnnotations rest a)
    "Expected class to be in nested, but no nested type exist."
    t
  B.TPathTypeArgument -> updateOrFail
    (classTypeArguments . ix (fromIntegral i))
    (addAnnotations addTypeArgumentAnnotations rest a)
    "Expected class to be in nested, but no nested type exist."
    t
  _ -> badPath tpi (show t)

addTypeArgumentAnnotations
  :: NonEmpty B.TypePathItem
  -> Annotation
  -> TypeArgument
  -> Either String TypeArgument
addTypeArgumentAnnotations (tpi@(B.TypePathItem B.TPathWildcard 0) :| rest) a t
  = case t of
    ExtendedTypeArg t' ->
      ExtendedTypeArg <$> addAnnotations addReferenceTypeAnnotations rest a t'
    ImplementedTypeArg t' ->
      ExtendedTypeArg <$> addAnnotations addReferenceTypeAnnotations rest a t'
    _ -> badPath tpi (show t)
addTypeArgumentAnnotations (tpi :| _) _ t = badPath tpi (show t)


getTypeAnnotations :: Annotated Type -> [(B.TypePath, Annotation)]
getTypeAnnotations _ = []
