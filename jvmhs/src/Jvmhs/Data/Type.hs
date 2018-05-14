{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module : Jvmhs.Data.Type
Copyright : (c) Christian Gram Kalhauge, 2018
License  : MIT
Maintainer : kalhauge@cs.ucla.edu

This module reexports the Types from the `jvm-binary` packages, and creates
lenses and toJSON instances for them.

This *will* create orhpaned instances, so do not import without

-}
module Jvmhs.Data.Type
  ( ClassName
  , dotCls
  , strCls
  , splitClassName
  , fullyQualifiedName
  , package
  , shorthand

  , MethodDescriptor (..)
  , methodDArguments
  , methodDReturnType

  , MethodId
  , methodId
  , methodIdName
  , methodIdDescriptor
  , methodIdToText

  , FieldDescriptor (..)
  , fieldDType

  , FieldId
  , fieldId
  , fieldIdName
  , fieldIdDescriptor
  , fieldIdToText

  , JType (..)
  , JValue (..)

  , MAccessFlag (..)
  , FAccessFlag (..)
  , CAccessFlag (..)
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Char
import qualified Data.Text               as Text
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Char8   as C

import           Language.JVM.AccessFlag
import           Language.JVM.Constant   hiding (FieldId, MethodId, MethodHandle)
import qualified Language.JVM.Constant   as B
import           Language.JVM.Type
-- import           Language.JVM.Utils

-- * ClassName

type Package = [ Text.Text ]

makeWrapped ''ClassName

fullyQualifiedName :: Iso' ClassName Text.Text
fullyQualifiedName = _Wrapped

-- | Splits a ClassName in it's components
splitClassName :: Iso' ClassName [Text.Text]
splitClassName =
  fullyQualifiedName . split
  where
    split = iso (Text.splitOn "/") (Text.intercalate "/")

-- | The package name of the class name
package :: Traversal' ClassName Package
package =
  splitClassName . _init

-- | The shorthand name of the class name
shorthand :: Traversal' ClassName Text.Text
shorthand =
  splitClassName . _last

-- * MethodDescriptor

-- | Get a the argument types from a method descriptor
methodDArguments :: Lens' MethodDescriptor [JType]
methodDArguments =
  lens
    methodDescriptorArguments
    (\md a -> md { methodDescriptorArguments = a })

-- | Get a the return type from a method descriptor
methodDReturnType :: Lens' MethodDescriptor (Maybe JType)
methodDReturnType =
  lens methodDescriptorReturnType
    (\md a -> md { methodDescriptorReturnType = a})


-- * FieldDescriptor

-- | Get the type from a field descriptor
fieldDType :: Iso' FieldDescriptor JType
fieldDType =
  coerced
{-# INLINE fieldDType #-}

-- fromText :: Iso' (Maybe Text.Text) (Maybe FieldDescriptor)
-- fromText =
--   iso B.fieldDescriptorFromText B.fieldDescriptorToText

-- * JType

-- * Value

type FieldId = B.FieldId High
type MethodId = B.MethodId High
type MethodHandle = B.MethodHandle High

methodId :: Text.Text -> MethodDescriptor -> MethodId
methodId = B.MethodId

fieldId :: Text.Text -> FieldDescriptor -> FieldId
fieldId = B.FieldId

instance ToJSON FieldId where
  toJSON = String . fieldIdToText

instance ToJSON MethodId where
  toJSON = String . methodIdToText

-- * Instances

instance ToJSON ClassName where
  toJSON = String . view fullyQualifiedName

instance FromJSON ClassName where
  parseJSON = withText "ClassName" (return . ClassName)

instance ToJSON FieldDescriptor where
  toJSON = String . fieldDescriptorToText

instance ToJSON MethodDescriptor where
  toJSON = String . methodDescriptorToText

instance ToJSON BS.ByteString where
  toJSON = String . Text.pack . C.unpack

instance ToJSON MethodHandle where
  toJSON _ = String "MethodHandle"


$(deriveToJSON (defaultOptions { constructorTagModifier = drop 1 }) ''CAccessFlag)
$(deriveToJSON (defaultOptions { constructorTagModifier = drop 1 }) ''FAccessFlag)
$(deriveToJSON (defaultOptions { constructorTagModifier = drop 1 }) ''MAccessFlag)
$(deriveToJSON (defaultOptions
                 { sumEncoding             = ObjectWithSingleField
                 , constructorTagModifier  = map toLower . drop 1
                 }
               ) ''JValue)
