module Language.JVM.Attribute
  ( Attribute (..)
  ) where

import           Data.ByteString       (ByteString)
import           Data.Word

import           Language.JVM.Constant (ConstantRef)

data Attribute = Attribute
  { attributeNameIndex :: ConstantRef
  , attributeLength    :: !Word32
  , info               :: ByteString
  }
