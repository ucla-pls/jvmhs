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

module Jvmhs.Data.Annotation
  ( Annotation
  , emptyAnnotations
  , TypeAnnotation(..)
  , emptyTypeAnnotation
  , Annotations(..)
  , invisibleAnnotations
  , visibleAnnotations
  , AnnotationMap

  -- * Annotatio nValues
  , AnnotationValue(..)

  -- * Annotated
  , Annotated(..)
  , annotatedContent
  , annotatedAnnotation

  -- ** Creation
  , withNoAnnotation

  -- * Re-exports
  , B.EnumValue(..)
  )
where

-- base
import           GHC.Generics                   ( Generic )

-- deep-seq
import           Control.DeepSeq

-- text
import qualified Data.Text                     as Text

-- lens
import           Control.Lens            hiding ( (.=) )

-- -- aeson
-- import           Data.Aeson

-- unordered-containers
import qualified Data.HashMap.Strict           as HashMap

-- jvm-binary
import qualified Language.JVM                  as B
import qualified Language.JVM.Attribute.Annotations
                                               as B

-- | Annotations can either be runtime visisible or invisible.
data Annotations = Annotations
  { _visibleAnnotations   :: ! AnnotationMap
  , _invisibleAnnotations :: ! AnnotationMap
  } deriving (Show, Eq, Generic, NFData)

emptyAnnotations :: Annotations
emptyAnnotations = Annotations { _visibleAnnotations   = HashMap.empty
                               , _invisibleAnnotations = HashMap.empty
                               }

data TypeAnnotation = TypeAnnotation
  { _visibleTypeAnnotation   :: ! Annotation
  , _invisibleTypeAnnotation :: ! Annotation
  } deriving (Show, Eq, Generic, NFData)

emptyTypeAnnotation :: TypeAnnotation
emptyTypeAnnotation = TypeAnnotation HashMap.empty HashMap.empty

-- | A type can be annotated.
data Annotated a = Annotated
  { _annotatedContent :: !a
  , _annotatedAnnotation :: TypeAnnotation
  } deriving (Show, Eq, Generic, NFData)

withNoAnnotation :: a -> Annotated a
withNoAnnotation a = Annotated a emptyTypeAnnotation


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

makeLenses ''Annotations
makePrisms ''AnnotationValue
makeLenses ''Annotated
