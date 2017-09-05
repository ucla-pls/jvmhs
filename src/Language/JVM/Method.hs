{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.JVM.Method
  ( Method (..)

  , accessFlags
  , name
  , descriptor
  , code

  , desc

  , MethodName
  , MethodId
  , identifier

  , C.MethodDescriptor (..)

  , B.AccessFlags (..)
  , fromBinary
  ) where

import Language.JVM.ClassName
import Data.Monoid

import qualified Language.JVM.Binary.Method as B
import qualified Language.JVM.Binary.Attribute as A
import qualified Language.JVM.Binary.Constant as C
import qualified Language.JVM.Binary.Code as Code
import qualified Language.JVM.Type as T

import qualified Data.Text as Text

import Control.Lens

-- -- import Data.Aeson
-- -- import Data.Aeson.TH

type MethodName = Text.Text

type MethodId = (MethodName, C.MethodDescriptor)

data Method = Method
  { _name :: MethodName
  , _descriptor :: C.MethodDescriptor
  , _accessFlags :: B.AccessFlags
  , _code :: Maybe Code.Code
  } deriving (Eq, Show)

makeLenses ''Method

identifier :: Getter Method MethodId
identifier = to $ \m -> (m ^. name, m ^. descriptor)

desc :: ClassName -> Method -> Text.Text
desc (ClassName cn) m =
  cn <> "." <> (m ^. name) <> ":" <> T.writeMethodDesciptor (m ^. descriptor)

fromBinary :: C.ConstantPool -> B.Method -> Maybe Method
fromBinary cp m = do
  mname <- C.lookupText (B.nameIndex m) cp
  mdescriptor <- C.lookupMethodDescriptor (B.descriptorIndex m) cp
  let mcode = B.attributes m ^? traverse
                           . filtered ((== Just "Code") . view (A.name cp))
                           . to Code.fromAttribute
                           . _Right
  return $ Method mname mdescriptor (B.accessFlags m) mcode

-- deriveJSON (defaultOptions { fieldLabelModifier = drop 1 }) ''Method
