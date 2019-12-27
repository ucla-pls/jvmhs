{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
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

-}
module Jvmhs.Data.Type
  (
  -- * Basic types 
    JType(..)
  , JBaseType(..)
  , JRefType(..)
  , ClassName(..)

  -- * Bigger types
  , Type(..)
  , ReferenceType(..)

  -- ** ClassType
  , ClassType(..)
  , classTypeName
  , classTypeArguments
  , classTypeAnnotation
  , classTypeInner
  , extendClassType
  , classTypeFromName
  , innerClassTypeFromName
  , classNameFromType

  -- ** Others
  , TypeParameter(..)
  , ThrowsSignature(..)
  , TypeVariable(..)
  , TypeArgument(..)
  , TypeArgumentDescription(..)
  , B.Wildcard(..)

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
import           GHC.Generics                   ( Generic )

-- deep-seq
import           Control.DeepSeq

-- text
import qualified Data.Text                     as Text

-- lens
import           Control.Lens            hiding ( (.=) )

-- -- aeson
-- import           Data.Aeson

-- jvm-binary
import qualified Language.JVM                  as B
import           Language.JVM.Type
import qualified Language.JVM.Attribute.Signature
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
  | RefArrayType !Type
  deriving (Show, Eq, Generic, NFData)

-- | A throw signature can also be annotated
data ThrowsSignature
  = ThrowsClass ! ClassType
  | ThrowsTypeVariable ! TypeVariable
  deriving (Show, Eq, Generic, NFData)

-- | An 'ClassType' is interesting because it can represent inner classes
-- in different ways.
data ClassType = ClassType
  { _classTypeName :: ! Text.Text
  , _classTypeInner :: ! (Maybe ClassType)
  , _classTypeArguments :: [ TypeArgument ]
  , _classTypeAnnotation :: TypeAnnotation
  } deriving (Show, Eq, Generic, NFData)

data TypeArgument
  = AnyType
  | TypeArgument !TypeArgumentDescription
  deriving (Show, Eq, Generic, NFData)

data TypeArgumentDescription = TypeArgumentDescription
  { _typeArgWildcard :: ! (Maybe B.Wildcard)
  , _typeArgType :: ! ReferenceType
  } deriving (Show, Eq, Generic, NFData)

newtype TypeVariable = TypeVariable
  { _typeVariable :: Text.Text
  } deriving (Show, Eq, Generic, NFData)

data Type
  = ReferenceType !ReferenceType
  | BaseType !JBaseType
  deriving (Show, Eq, Generic, NFData)

makeLenses ''ClassType
makePrisms ''ThrowsSignature
makePrisms ''ReferenceType
makePrisms ''Type

-- | Get the name of a class type, this throws away all anotations and 
-- type signatures
classNameFromType :: ClassType -> ClassName
classNameFromType ct =
  fromRight (error "Unexpected behaviour, please report a bug")
    $ B.textCls (Text.intercalate "$" $ nameOf ct)
  where nameOf t = t ^. classTypeName : maybe [] nameOf (t ^. classTypeInner)

-- | Extend a ClassType with an inner classType.
extendClassType :: ClassType -> ClassType -> ClassType
extendClassType ct =
  classTypeInner
    %~ (\case
         Just x  -> Just $ extendClassType ct x
         Nothing -> Just $ ct
       )

-- | Creates a ClassType without any annotations and typesignatures
classTypeFromName :: ClassName -> ClassType
classTypeFromName = innerClassTypeFromName . classNameAsText

-- | Creates a ClassType without any annotations and typesignatures
innerClassTypeFromName :: Text.Text -> ClassType
innerClassTypeFromName = fromJust . go . Text.split (== '$')
 where
  go []       = Nothing
  go (a : as) = Just $ ClassType a (go as) [] emptyTypeAnnotation

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


-- | Create a 'ReferenceType' from a 'JRefType', without any annoations 
-- or generics.
fromJRefType :: B.JRefType -> ReferenceType
fromJRefType = \case
  JTClass cn  -> RefClassType $ classTypeFromName cn
  JTArray atp -> RefArrayType $ fromJType atp

-- | Convert a ReferenceType to either a 'TypeVariable' or a simple
-- 'B.JRefType'.
toJRefType :: ReferenceType -> Either TypeVariable B.JRefType
toJRefType = \case
  RefClassType    ct  -> Right $ JTClass (classNameFromType ct)
  RefArrayType    atp -> JTArray <$> toJType atp
  RefTypeVariable tv  -> Left tv

-- addTypeAnnotation :: [B.TypePathItem] -> Annotation -> Type -> Type
-- addTypeAnnotation tpi ann tp = case tpi of
--   [] -> tp & annotation .~ ann
