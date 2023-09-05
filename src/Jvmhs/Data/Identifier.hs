{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module : Jvmhs.Data.Identifier
Copyright : (c) Christian Gram Kalhauge, 2018-2019
License  : BSD3
Maintainer : kalhauge@cs.ucla.edu

This module reexports the identifies from the `jvm-binary` packages, and
creates lenses and toJSON instances for them.
-}
module Jvmhs.Data.Identifier (
  FromJVMBinary (..),

  -- * ClassName
  ClassName,
  HasClassName (..),
  dotCls,
  textCls,
  splitClassName,
  fullyQualifiedName,
  isInnerClass,
  package,
  shorthand,

  -- * MethodId
  MethodId (..),
  mkMethodId,
  HasMethodId (..),
  MethodDescriptor (..),
  methodDArguments,
  methodDReturnType,
  ReturnDescriptor (..),

  -- * FieldId
  FieldId (..),
  mkFieldId,
  HasFieldId (..),
  FieldDescriptor (..),

  -- * InClass and InRefType
  InClass (..),
  inClassNameL,
  inClassIdL,
  InRefType (..),
  inRefTypeL,
  inRefTypeIdL,
  asInClass,
  AbsMethodId (..),
  mkAbsMethodId,
  AbsFieldId (..),
  mkAbsFieldId,

  -- * Access Flags
  MAccessFlag (..),
  FAccessFlag (..),
  CAccessFlag (..),
  ICAccessFlag (..),
  PAccessFlag (..),

  -- * Re-exports

  -- , B.JValue(..)
  -- , JType(..)
  -- , JBaseType(..)
  -- , JRefType(..)
  AsNameAndType (..),
  WithName (..),
  module Language.JVM.TextSerializable,
) where

-- lens
import Control.Lens

-- aeson
import Data.Aeson
import Data.Aeson.Encoding (text)
import qualified Data.Aeson.Key as Key
import Data.Aeson.TH
import Data.Aeson.Types (Parser)

-- cassava
import qualified Data.Csv as Csv

-- bytestring
import qualified Data.ByteString as BS

-- hashable
import Data.Hashable

-- base
import Data.Char (toLower)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as Text

-- jvm-binary

import qualified Data.Text.Encoding as Text
import Language.JVM.AccessFlag
import qualified Language.JVM.Constant as B
import Language.JVM.TextSerializable
import Language.JVM.Type

-- cones
import Data.Cone.TH

makeWrapped ''FieldId
makeWrapped ''MethodId
makeWrapped ''ReturnDescriptor
makeWrapped ''AbsFieldId
makeWrapped ''AbsMethodId
makeWrapped ''ClassName

-- * Wrap
class FromJVMBinary b n | n -> b where
  _Binary :: Iso' n b

class HasClassName a where
  className :: Lens' a ClassName

instance HasClassName ClassName where
  className = id
  {-# INLINE className #-}

instance Hashable ClassName where
  hashWithSalt i a = i `hashWithSalt` view _Wrapped a

fullyQualifiedName :: Iso' ClassName Text.Text
fullyQualifiedName = _Wrapped
{-# INLINEABLE fullyQualifiedName #-}

-- | Splits a ClassName in it's components
splitClassName :: Iso' ClassName (NE.NonEmpty Text.Text)
splitClassName = fullyQualifiedName . split
 where
  split =
    iso
      (fromJust . NE.nonEmpty . Text.splitOn "/")
      (Text.intercalate "/" . NE.toList)
{-# INLINEABLE splitClassName #-}

-- | Checks if an class is an Inner Class
isInnerClass :: ClassName -> Bool
isInnerClass = Text.any (== '$') . view fullyQualifiedName

type Package = [Text.Text]

-- | The package name of the class name
package :: Lens' ClassName Package
package = splitClassName . lens NE.init (\a b -> NE.reverse (NE.last a NE.:| reverse b))
{-# INLINEABLE package #-}

-- | The shorthand name of the class name
shorthand :: Lens' ClassName Text.Text
shorthand = splitClassName . lens NE.last (\a b -> NE.reverse (b NE.<| NE.reverse a))
{-# INLINEABLE shorthand #-}

instance ToJSON ClassName where
  toJSON = String . view fullyQualifiedName

instance ToJSONKey ClassName where
  toJSONKey = ToJSONKeyText (Key.fromText . f) (text . f) where f = view fullyQualifiedName

-- * NameAndType

instance Hashable a => Hashable (B.NameAndType a) where
  hashWithSalt i (B.NameAndType a b) = i `hashWithSalt` a `hashWithSalt` b

ntNameL :: Lens' (NameAndType a) Text.Text
ntNameL = lens ntName (\(NameAndType _ b) a -> NameAndType a b)

ntDescriptorL :: Lens' (NameAndType a) a
ntDescriptorL = lens ntDescriptor (\(NameAndType a _) b -> NameAndType a b)

-- * MethodId

mkMethodId :: Text.Text -> MethodDescriptor -> MethodId
mkMethodId = (<:>)

-- | Get a the argument types from a method descriptor
methodDArguments :: Lens' MethodDescriptor [JType]
methodDArguments =
  lens methodDescriptorArguments (\md a -> md{methodDescriptorArguments = a})
{-# INLINE methodDArguments #-}

-- | Get a the return type from a method descriptor
methodDReturnType :: Lens' MethodDescriptor (Maybe JType)
methodDReturnType =
  lens
    methodDescriptorReturnType
    (\md a -> md{methodDescriptorReturnType = a})
    . _Wrapped
{-# INLINE methodDReturnType #-}

instance Hashable JBaseType where
  hashWithSalt i a = i `hashWithSalt` jBaseTypeToChar a

instance Hashable ReturnDescriptor where
  hashWithSalt i (ReturnDescriptor a) = i `hashWithSalt` a

instance Hashable MethodDescriptor where
  hashWithSalt i (MethodDescriptor a b) = i `hashWithSalt` a `hashWithSalt` b

class HasMethodId a where
  methodId :: Getter a MethodId

  methodIdName :: Getter a Text.Text
  methodIdName = methodId . _Wrapped . ntNameL
  {-# INLINE methodIdName #-}

  methodIdDescriptor :: Getter a MethodDescriptor
  methodIdDescriptor = methodId . _Wrapped . ntDescriptorL
  {-# INLINE methodIdDescriptor #-}

  -- | Get the type of field
  methodIdArgumentTypes :: Getter a [JType]
  methodIdArgumentTypes =
    methodIdDescriptor . methodDArguments
  {-# INLINE methodIdArgumentTypes #-}

  -- | Get the return type
  methodIdReturnType :: Getter a (Maybe JType)
  methodIdReturnType =
    methodIdDescriptor . methodDReturnType
  {-# INLINE methodIdReturnType #-}

instance HasMethodId MethodId where
  methodId = id

instance Hashable MethodId where
  hashWithSalt i a = i `hashWithSalt` view _Wrapped a

-- * FieldId

-- | Get the type from a field descriptor
fieldDType :: Iso' FieldDescriptor JType
fieldDType = coerced
{-# INLINE fieldDType #-}

mkFieldId :: Text.Text -> FieldDescriptor -> FieldId
mkFieldId = (<:>)

class HasFieldId a where
  fieldId :: Getter a FieldId

  fieldIdName :: Getter a Text.Text
  fieldIdName = fieldId . _Wrapped . ntNameL
  {-# INLINE fieldIdName #-}

  fieldIdDescriptor :: Getter a FieldDescriptor
  fieldIdDescriptor = fieldId . _Wrapped . ntDescriptorL
  {-# INLINE fieldIdDescriptor #-}

  -- | Get the type of field
  fieldIdType :: Getter a JType
  fieldIdType = fieldIdDescriptor . fieldDType
  {-# INLINE fieldIdType #-}

instance HasFieldId FieldId where
  fieldId = id
  {-# INLINE fieldId #-}

  fieldIdName = _Wrapped . ntNameL
  {-# INLINE fieldIdName #-}

  fieldIdDescriptor = _Wrapped . ntDescriptorL
  {-# INLINE fieldIdDescriptor #-}

instance Hashable FieldId where
  hashWithSalt i a = i `hashWithSalt` view _Wrapped a

instance Hashable FieldDescriptor where
  hashWithSalt i (FieldDescriptor a) = i `hashWithSalt` a

-- * JType

instance Hashable JRefType where
  hashWithSalt i = \case
    JTArray b -> i `hashWithSalt` b
    JTClass b -> i `hashWithSalt` b

instance Hashable JType where
  hashWithSalt i = \case
    JTBase b -> i `hashWithSalt` b
    JTRef b -> i `hashWithSalt` b

-- * InClass

inClassNameL :: Lens' (InClass a) ClassName
inClassNameL = lens inClassName (\a b -> a{inClassName = b})

inClassIdL :: Lens (InClass a) (InClass b) a b
inClassIdL = lens inClassId (\a b -> a{inClassId = b})

instance HasClassName (InClass a) where
  className = inClassNameL

-- * InRefType

inRefTypeL :: Lens' (InRefType a) JRefType
inRefTypeL = lens inRefType (\a b -> a{inRefType = b})

inRefTypeIdL :: Lens (InRefType a) (InRefType b) a b
inRefTypeIdL = lens inRefTypeId (\a b -> a{inRefTypeId = b})

asInClass
  :: (Profunctor p, Contravariant f) => Optic' p f (InRefType a) (InClass a)
asInClass = to inRefTypeAsInClass

mkAbsFieldId :: (HasClassName b, HasFieldId a) => b -> a -> AbsFieldId
mkAbsFieldId cn a = AbsFieldId $ InClass (cn ^. className) (a ^. fieldId)

instance HasFieldId AbsFieldId where
  fieldId = _Wrapped . inClassIdL

instance HasClassName AbsFieldId where
  className = _Wrapped . inClassNameL

instance HasFieldId (InRefType FieldId) where
  fieldId = inRefTypeIdL

mkAbsMethodId :: (HasClassName b, HasMethodId a) => b -> a -> AbsMethodId
mkAbsMethodId cn a = AbsMethodId $ InClass (cn ^. className) (a ^. methodId)

instance HasMethodId AbsMethodId where
  methodId = _Wrapped . inClassIdL

instance HasClassName AbsMethodId where
  className = _Wrapped . inClassNameL

instance HasMethodId (InRefType MethodId) where
  methodId = inRefTypeIdL

parserFromEither :: Either String a -> Parser a
parserFromEither = either error return

-- -- * Instances

instance ToJSON JType where
  toJSON = String . serialize

instance Csv.ToField AbsMethodId where
  toField = Csv.toField . serialize

instance Csv.ToField AbsFieldId where
  toField = Csv.toField . serialize

instance FromJSON JType where
  parseJSON = withText "JType" (parserFromEither . deserialize)

instance ToJSON JRefType where
  toJSON = String . serialize

instance FromJSON JRefType where
  parseJSON = withText "JRefType" (parserFromEither . deserialize)

instance ToJSON FieldId where
  toJSON = String . serialize

instance ToJSON MethodId where
  toJSON = String . serialize

instance ToJSON AbsFieldId where
  toJSON = String . serialize

instance ToJSON AbsMethodId where
  toJSON = String . serialize

instance FromJSON FieldId where
  parseJSON = withText "FieldId" (parserFromEither . deserialize)

instance FromJSON MethodId where
  parseJSON = withText "MethodId" (parserFromEither . deserialize)

instance ToJSONKey FieldId where
  toJSONKey = ToJSONKeyText (Key.fromText . serialize) (text . serialize)

instance FromJSONKey FieldId where
  fromJSONKey = FromJSONKeyTextParser (parserFromEither . deserialize)

instance ToJSONKey MethodId where
  toJSONKey = ToJSONKeyText (Key.fromText . serialize) (text . serialize)

instance FromJSONKey MethodId where
  fromJSONKey = FromJSONKeyTextParser (parserFromEither . deserialize)

instance FromJSON ClassName where
  parseJSON = withText "ClassName" (parserFromEither . deserialize)

instance FromJSONKey ClassName where
  fromJSONKey = FromJSONKeyTextParser (parserFromEither . deserialize)

instance ToJSON FieldDescriptor where
  toJSON = String . serialize

instance FromJSON FieldDescriptor where
  parseJSON = withText "FieldDescriptor" (parserFromEither . deserialize)

instance ToJSON MethodDescriptor where
  toJSON = String . serialize

instance FromJSON MethodDescriptor where
  parseJSON = withText "MethodDescriptor" (parserFromEither . deserialize)

-- | TODO we should not be deriving these classes.
instance ToJSON BS.ByteString where
  toJSON = String . Text.decodeUtf8

instance FromJSON BS.ByteString where
  parseJSON = withText "VString" (pure . Text.encodeUtf8)

instance ToJSON ReturnDescriptor where
  toJSON = String . serialize

instance FromJSON ReturnDescriptor where
  parseJSON = withText "ReturnDescriptor" (parserFromEither . deserialize)

instance ToJSON (B.MethodHandle B.High) where
  toJSON _ = String "MethodHandle"

instance FromJSON (B.MethodHandle B.High) where
  parseJSON _ = fail "Not Yet Implemented"

$(deriveJSON (defaultOptions{constructorTagModifier = fmap toLower . drop 1}) ''CAccessFlag)
$(deriveJSON (defaultOptions{constructorTagModifier = fmap toLower . drop 1}) ''FAccessFlag)
$(deriveJSON (defaultOptions{constructorTagModifier = fmap toLower . drop 1}) ''MAccessFlag)
$(deriveJSON (defaultOptions{constructorTagModifier = fmap toLower . drop 1}) ''PAccessFlag)
$(deriveJSON (defaultOptions{constructorTagModifier = fmap toLower . drop 1}) ''ICAccessFlag)
$( deriveJSON
    ( defaultOptions
        { sumEncoding = ObjectWithSingleField
        , constructorTagModifier = camelTo2 '_' . drop 1
        }
    )
    ''B.JValue
 )

$(makeDiagram ''CAccessFlag)
$(makeDiagram ''MAccessFlag)
$(makeDiagram ''FAccessFlag)
$(makeDiagram ''ICAccessFlag)
$(makeDiagram ''PAccessFlag)
