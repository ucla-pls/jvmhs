{-# LANGUAGE TemplateHaskell #-}
module Language.JVM.Field
  ( Field (..)

  , name
  , accessFlags
  , descriptor

  , fromBinary
  ) where

import qualified Language.JVM.Binary.Constant as C
import qualified Language.JVM.Binary.Field    as B

import qualified Data.Text                    as Text

import Control.Lens

import Data.Aeson
import Data.Aeson.TH

type FieldName = Text.Text

data Field = Field
  { _accessFlags :: B.AccessFlags
  , _name        :: FieldName
  , _descriptor  :: C.FieldDescriptor
  } deriving (Eq, Show)

makeLenses ''Field

fromBinary :: C.ConstantPool -> B.Field -> Maybe Field
fromBinary cp f = do
  mname <- C.lookupText (B.nameIndex f) cp
  mdescriptor <- C.lookupFieldDescriptor (B.descriptorIndex f) cp
  return $ Field (B.accessFlags f) mname mdescriptor

deriveJSON (defaultOptions { fieldLabelModifier = drop 1 }) ''Field
