{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE EmptyCase        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Jvmhs.Data.Annotation
Copyright   : (c) Christian Gram Kalhauge, 2019
License     : BSD3
Maintainer  : kalhauge@cs.ucla.edu
-}

module Jvmhs.Data.Annotation where

-- base
import           GHC.Generics                   ( Generic )
import           Data.Foldable
import           Data.Char                      ( chr )

-- deep-seq
import           Control.DeepSeq

-- text
-- import qualified Data.Text as Text

-- lens
import           Control.Lens            hiding ( (.=) )

-- aeson
import           Data.Aeson

-- unordered-containers
-- import qualified Data.HashMap.Strict                     as HashMap

-- jvm-binary
import qualified Language.JVM                  as B
import qualified Language.JVM.Attribute.Annotations
                                               as B

import           Jvmhs.Data.Type                ( )

type Annotation = B.Annotation B.High

data Annotations = Annotations
  { _visibleAnnotations :: [Annotation]
  , _invisibleAnnotations :: [Annotation]
  } deriving (Show, Eq, Generic, NFData)

instance ToJSON (Annotations) where
  toJSON Annotations {..} = object
    ["visible" .= _visibleAnnotations, "invisible" .= _invisibleAnnotations]

instance ToJSON (B.Annotation B.High) where
  toJSON B.Annotation {..} =
    object ["type" .= annotationType, "content" .= annotationValuePairs]

  toEncoding B.Annotation {..} =
    pairs ("type" .= annotationType <> "content" .= annotationValuePairs)

instance ToJSON (B.SizedList16 (B.ValuePair B.High)) where
  toJSON a = object . map (\(B.ValuePair n v) -> n .= v) $ toList a

instance ToJSON (B.ElementValue B.High) where
  toJSON = \case
    B.EByte           a                 -> object ["byte" .= a]
    B.EChar a -> object ["char" .= chr (fromIntegral a)]
    B.EDouble         a                 -> object ["double" .= a]
    B.EFloat          a                 -> object ["float" .= a]
    B.EInt            a                 -> object ["int" .= a]
    B.ELong           a                 -> object ["long" .= a]
    B.EShort          a                 -> object ["short" .= a]
    B.EBoolean        a                 -> object ["boolean" .= (a /= 0)]
    B.EString         a                 -> object ["string" .= a]
    B.EEnum (B.EnumValue a b) -> object ["enum" .= b, "enum_class" .= a]
    B.EClass          a                 -> object ["class_info" .= a]
    B.EAnnotationType a                 -> object ["annotation" .= a]
    B.EArrayType      a                 -> object ["array" .= toList a]


makeLenses ''Annotations
