{-# LANGUAGE TemplateHaskell #-}
module Language.JVM.Method
  ( Method (..)
  , B.AccessFlags (..)
  , fromBinary
  ) where

import qualified Language.JVM.Binary.Method as B
import qualified Language.JVM.Binary.Constant as C

import qualified Data.Text as Text

import Data.Aeson
import Data.Aeson.TH

type MethodName = Text.Text

data Method = Method
  { accessFlags :: B.AccessFlags
  , name :: MethodName
  , descriptor :: C.MethodDescriptor
  } deriving (Eq, Show)


fromBinary :: C.ConstantPool -> B.Method -> Maybe Method
fromBinary cp m = do
  name <- C.lookupText (B.nameIndex m) cp
  descriptor <- C.lookupMethodDescriptor (B.descriptorIndex m) cp
  return $ Method (B.accessFlags m) name descriptor

deriveJSON defaultOptions ''Method
