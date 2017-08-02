module Language.JVM.Method
  ( Method (..)
  ) where

import           Data.Word

import           Language.JVM.Constant (ConstantRef)
import           Language.JVM.Attribute (Attribute)

data Method = Method
  { accessFlags     :: !Word16
  , nameIndex       :: ConstantRef
  , descriptorIndex :: ConstantRef
  , attributeCount  :: !Word16
  , attributes      :: [Attribute]
  }
