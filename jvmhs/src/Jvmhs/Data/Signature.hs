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
  , classTypeFromName
  , throwsSignatureFromName
  , throwsSignatureName

  , typeSignatureFromType

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

-- base
import qualified Data.List as List

-- jmvhs
import Jvmhs.Data.Type

-- jvm-binary
import Language.JVM.Attribute.Signature

makePrisms ''ThrowsSignature

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
        [ctsClassName ^. fullyQualifiedName]

classTypeFromName :: ClassName -> ClassType
classTypeFromName cn =
  case Text.split (== '$') (view fullyQualifiedName cn) of
    t : rst -> List.foldl'
      (\b t' -> InnerClassType t' b [])
      (ClassType (review fullyQualifiedName t) [])
      rst
    [] -> error "Can't happen."

throwsSignatureFromName :: ClassName -> ThrowsSignature
throwsSignatureFromName cn =
  ThrowsClass (classTypeFromName cn)

throwsSignatureName :: Monoid f => Getting f ThrowsSignature ClassName
throwsSignatureName =
  _ThrowsClass . classTypeName

referenceTypeFromRefType :: JRefType -> ReferenceType
referenceTypeFromRefType = \case
  JTArray a -> RefArrayType (typeSignatureFromType a)
  JTClass a -> RefClassType (classTypeFromName a)

typeSignatureFromType :: JType -> TypeSignature
typeSignatureFromType = \case
  JTBase a -> BaseType a
  JTRef a  -> ReferenceType (referenceTypeFromRefType a)



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

instance ToJSON (ThrowsSignature) where
  toJSON = String . LazyText.toStrict . toLazyText . throwsSignatureT

instance ToJSON (TypeSignature) where
  toJSON = String . LazyText.toStrict . toLazyText . typeSignatureT
