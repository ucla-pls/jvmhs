module Language.JVM.Field
  ( Field (..)
  ) where

import Data.Word

import Language.JVM.Attribute (Attribute)
import Language.JVM.Constant (ConstantRef)

data Field = Field
  { accessFlags :: !Word16
  , nameIndex :: ConstantRef
  , discriptorIndex :: ConstantRef
  , attributesCount :: !Word16
  , attributes :: [Attribute]
  }
