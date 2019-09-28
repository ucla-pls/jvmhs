{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Jvmhs.Data.ClassType
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD-3-Clause
Maintainer  : kalhauge@cs.ucla.edu

ClassType is a combination of the Signature, Annotation, and the

simple ClassName.

-}

module Jvmhs.Data.Signature
  ( classTypeName
  , module Language.JVM.Attribute.Signature
  ) where

-- aeson
import Data.Aeson

-- lens
import Control.Lens

-- text
import qualified Data.Text as Text
import Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy as LazyText

-- jmvhs
import Jvmhs.Data.Type

-- jvm-binary
import Language.JVM.Attribute.Signature

-- | A classType getter from a ClassType
classTypeName :: Getting f ClassType ClassName
classTypeName = Control.Lens.to getClassTypeName

getClassTypeName :: ClassType -> ClassName
getClassTypeName =
  (review fullyQualifiedName . Text.intercalate "$" . reverse . getClassName)
  where
    getClassName = \case
      InnerClassType {..} ->
        ctsInnerClassName : getClassName ctsOuterClassType
      ClassType {..} ->
        [ctsClassName ^. from _Binary . fullyQualifiedName]


instance ToJSON (ClassSignature) where
  toJSON = String . classSignatureToText

instance ToJSON (MethodSignature) where
  toJSON = String . methodSignatureToText

instance ToJSON (FieldSignature) where
  toJSON = String . fieldSignatureToText

instance ToJSON (TypeParameter) where
  toJSON = String . LazyText.toStrict . toLazyText . typeParameterT

instance ToJSON (ClassType) where
  toJSON = String . LazyText.toStrict . toLazyText . classTypeT

