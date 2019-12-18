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
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Jvmhs.Data.Annotation
Copyright   : (c) Christian Gram Kalhauge, 2019
License     : BSD3
Maintainer  : kalhauge@cs.ucla.edu

This module defines the three annotated types `AnnotatedClassType` and
`AnnotatedReferenceType`  and `AnnotatedThrowsSignature`. These modules are
esentially extensions of the signatures defined in
`Language.JVM.Attribute.Signature` to contain annotations.

-}

module Jvmhs.Data.Annotation where

-- base
import           Data.Char                      ( chr )
import           GHC.Generics                   ( Generic )

-- deep-seq
import           Control.DeepSeq

-- text
import qualified Data.Text                     as Text

-- lens
import           Control.Lens            hiding ( (.=) )

-- aeson
import           Data.Aeson

-- unordered-containers
import qualified Data.HashMap.Strict           as HashMap

-- jvm-binary
import qualified Language.JVM                  as B
import qualified Language.JVM.Attribute.Annotations
                                               as B
import qualified Language.JVM.Attribute.Signature
                                               as B

import           Jvmhs.Data.Type

-- | Annotations can either be runtime visisible or invisible.
data Annotations = Annotations
  { _visibleAnnotations   :: ! AnnotationMap
  , _invisibleAnnotations :: ! AnnotationMap
  } deriving (Show, Eq, Generic, NFData)

emptyAnnotations :: Annotations
emptyAnnotations = Annotations { _visibleAnnotations   = HashMap.empty
                               , _invisibleAnnotations = HashMap.empty
                               }

-- | An annotation map is a map of annotation types to annotation objects.
type AnnotationMap = HashMap.HashMap Text.Text Annotation

-- | A annotation is a map of names to values.
type Annotation = HashMap.HashMap Text.Text AnnotationValue

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
  | AAnnotation !(Text.Text, Annotation)
  | AArray !([ AnnotationValue ])
  deriving (Show, Eq, Generic, NFData)

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
  { _ClassTypeName :: ! Text.Text
  , _ClassTypeBase :: ! (Maybe ClassType)
  , _ClassTypeArguments :: [ TypeArgument ]
  } deriving (Show, Eq, Generic, NFData)

data TypeArgument
  = AnyType
  | TypeArgument !TypeArgumentDescription
  deriving (Show, Eq, Generic, NFData)

type Wildcard = B.Wildcard

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


instance ToJSON Annotations where
  toJSON Annotations {..} = object
    ["visible" .= _visibleAnnotations, "invisible" .= _invisibleAnnotations]

instance ToJSON AnnotationValue where
  toJSON = \case
    AByte       a                 -> object ["byte" .= a]
    AChar       a                 -> object ["char" .= chr (fromIntegral a)]
    ADouble     a                 -> object ["double" .= a]
    AFloat      a                 -> object ["float" .= a]
    AInt        a                 -> object ["int" .= a]
    ALong       a                 -> object ["long" .= a]
    AShort      a                 -> object ["short" .= a]
    ABoolean    a                 -> object ["boolean" .= (a /= 0)]
    AString     a                 -> object ["string" .= a]
    AEnum       (B.EnumValue a b) -> object ["enum" .= b, "enum_class" .= a]
    AClass      a                 -> object ["class_info" .= a]
    AAnnotation (name, a) -> object ["annotation" .= name, "values" .= a]
    AArray      a                 -> object ["array" .= a]

makePrisms ''AnnotationValue
makePrisms ''ThrowsSignature
makePrisms ''ReferenceType
makePrisms ''Type
