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

-- base
import GHC.Generics (Generic)

-- aeson
import Data.Aeson

-- deepseq
import Control.DeepSeq

-- lens
import Control.Lens

-- text
import qualified Data.Text as Text
import Data.Text.Lazy.Builder as Text
import qualified Data.Text.Lazy as LazyText

-- jmvhs
import Jvmhs.Data.Type

-- jvm-binary
import qualified Language.JVM as B
import Language.JVM.Attribute.Signature

-- -- | The class type holds both the full signature and annotation.
-- data ClassType
--   = ClassTypeBase BaseClassType
--   | ClassTypeInner InnerClassType
--   deriving (Show, Eq, Generic, NFData)

-- -- | A inner class
-- data InnerClassType = InnerClassType
--   { _innerClassTypeName      :: Text.Text
--   , _innerClassTypeArgument  :: [Maybe TypeArgument]
--   , _innerClassTypeOuterType :: ClassType
--   }
--   deriving (Show, Eq, Generic, NFData)

-- -- | A base class
-- data BaseClassType = BaseClassType
--   { _baseClassTypeName     :: ClassName
--   , _baseClassTypeArgument :: [Maybe TypeArgument]
--   }
--   deriving (Show, Eq, Generic, NFData)

-- type Wildcard = B.Wildcard

-- -- | The type argument
-- data TypeArgument = TypeArgument
--   { _typeArgumentWildcard :: Maybe B.Wildcard
--   , _typeArgumentType     :: ReferenceType
--   }
--   deriving (Show, Eq, Generic, NFData)

-- type TypeVariable = B.TypeVariable

-- data ReferenceType
--   = ReferenceClassType ClassType
--   | ReferenceTypeVariable TypeVariable
--   | ReferenceArrayType TypeSignature
--   deriving (Show, Eq, Generic, NFData)

-- data TypeSignature
--   = ReferenceType ReferenceType
--   | BaseType B.JBaseType
--   deriving (Show, Eq, Generic, NFData)

-- type TypeParameter = B.TypeParameter
-- type MethodSignature = B.MethodSignature

-- type FieldSignature = B.FieldSignature

-- fieldSignatureFromText = B.fieldSignatureFromText
-- fieldSignatureToText = B.fieldSignatureToText

-- methodSignatureFromText = B.methodSignatureFromText
-- methodSignatureToText = B.methodSignatureToText

-- makePrisms ''ClassType
-- makePrisms ''ReferenceType
-- makePrisms ''TypeSignature
-- makePrisms ''B.Wildcard

-- makeLenses ''InnerClassType
-- makeLenses ''BaseClassType

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

typeParameterT :: TypeParameter -> Builder
typeParameterT (TypeParameter n cb ibs) =
  fromText n <> singleton ':' <> maybe mempty referenceTypeT cb <>
    foldMap (\i -> singleton ':' <> referenceTypeT i) ibs


classSignatureT :: ClassSignature -> Builder
classSignatureT (ClassSignature tp ct its)= do
  typeParametersT tp <> foldMap classTypeT (ct:its)


typeSignatureT :: TypeSignature -> Builder
typeSignatureT (ReferenceType t) = referenceTypeT t
typeSignatureT (BaseType t)      = singleton (B.jBaseTypeToChar t)

referenceTypeT :: ReferenceType -> Builder
referenceTypeT t =
  case t of
    RefClassType ct    -> classTypeT ct
    RefTypeVariable tv -> typeVariableT tv
    RefArrayType at    -> singleton '[' <> typeSignatureT at

classTypeT :: ClassType -> Builder
classTypeT t =
  go t <> singleton ';'
  where
    go t' =
      case t' of
        InnerClassType n ct arg ->
          go ct <> singleton '.' <> Text.fromText n <> typeArgumentsT arg
        ClassType cn arg ->
          singleton 'L'
          <> Text.fromText (B.classNameAsText cn)
          <> typeArgumentsT arg

typeArgumentsT :: [ Maybe TypeArgument ] -> Builder
typeArgumentsT = \case
  [] -> mempty
  args -> singleton '<' <> foldMap typeArgumentT args <> singleton '>'

typeArgumentT :: Maybe TypeArgument -> Builder
typeArgumentT a = do
  case a of
    Nothing -> singleton '*'
    Just (TypeArgument w rt) ->
      (case w of
        Just WildMinus -> singleton '-'
        Just WildPlus  -> singleton '+'
        Nothing        -> mempty) <> referenceTypeT rt

typeVariableT :: TypeVariable -> Builder
typeVariableT (TypeVariable t)= do
  singleton 'T' <> Text.fromText t <> singleton ';'

typeParametersT :: [ TypeParameter ] -> Builder
typeParametersT = \case
  [] -> mempty
  args -> singleton '<' <> foldMap typeParameterT args <> singleton '>'

methodSignatureT :: MethodSignature -> Builder
methodSignatureT (MethodSignature tp args res thrws)= do
  typeParametersT tp
    <> singleton '('
    <> foldMap typeSignatureT args
    <> singleton ')'
    <> (case res of Nothing -> singleton 'V'; Just r -> typeSignatureT r)
    <> foldMap throwsSignatureT thrws

throwsSignatureT :: ThrowsSignature -> Builder
throwsSignatureT t =
  singleton '^'
    <> case t of
         ThrowsClass ct        -> classTypeT ct
         ThrowsTypeVariable tt -> typeVariableT tt
