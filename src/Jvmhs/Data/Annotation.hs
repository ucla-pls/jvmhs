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

-}

module Jvmhs.Data.Annotation
  ( Annotation(..)
  , annotationType
  , annotationIsRuntimeVisible
  , annotationValues
  , AnnotationMap
  , Annotations

  -- * Annotation Values
  , AnnotationValue(..)

  -- * Annotated
  , Annotated(..)
  , annotatedContent
  , annotatedAnnotations

  -- ** Creation
  , withNoAnnotation
  , getAnnotation

  -- * Re-exports
  , B.EnumValue(..)
  )
where

-- base
import           GHC.Generics                   ( Generic )

-- deep-seq
import           Control.DeepSeq

-- lens
import           Control.Lens            hiding ( (.=) )

import qualified Data.Text                     as Text

-- unordered-containers
import qualified Data.HashMap.Strict           as HashMap

import           Jvmhs.Data.Identifier

-- jvm-binary
import qualified Language.JVM                  as B
import qualified Language.JVM.Attribute.Annotations
                                               as B

-- | An annotation is a map of names to values.
data Annotation = Annotation
  { _annotationType             :: !ClassName
  -- ^ any field descriptor type is allowed here, but in practice only
  -- classes are used.
  , _annotationIsRuntimeVisible :: !Bool
  , _annotationValues           :: !AnnotationMap
  } deriving (Show, Eq, Generic, NFData)

  -- | An annotation map is a map of annotation types to annotation objects.
type AnnotationMap = HashMap.HashMap Text.Text AnnotationValue

-- | We collect annotations in a list.
type Annotations = [Annotation]

-- | A type can be annotated.
data Annotated a = Annotated
  { _annotatedContent :: !a
  , _annotatedAnnotations :: [Annotation]
  -- ^ It is assumed that the list does not contain dublicates.
  } deriving (Show, Eq, Generic, NFData)


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
  | AAnnotation !(ClassName, AnnotationMap)
  -- ^ Almost a complete annotation without the information about
  -- visibility this information is inheirited from
  | AArray ![ AnnotationValue ]
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

makeLenses ''Annotation
makePrisms ''AnnotationValue
makeLenses ''Annotated

-- | Create an annotated value with no annotations
withNoAnnotation :: a -> Annotated a
withNoAnnotation a = Annotated a []

-- | Get an annotation with ClassName
getAnnotation :: ClassName -> Annotated a -> Maybe Annotation
getAnnotation cn =
  findOf (annotatedAnnotations . folded) (view $ annotationType . to (== cn))
