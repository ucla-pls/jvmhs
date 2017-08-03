module Language.JVM.Binary.Method
  ( Method (..)
  ) where

import Data.Binary
import Data.Binary.Get

import qualified Data.Vector as V

import           Language.JVM.Binary.Constant (ConstantRef, getConstantRef, putConstantRef)
import           Language.JVM.Binary.Attribute (Attribute)
import           Language.JVM.Binary.Helpers

data Method = Method
  { accessFlags     :: !Word16
  , nameIndex       :: ConstantRef
  , descriptorIndex :: ConstantRef
  , attributes      :: V.Vector Attribute
  } deriving (Show, Eq)

instance Binary Method where
  get = Method
    <$> getWord16be
    <*> getConstantRef
    <*> getConstantRef
    <*> getVector

  put method = sequence_ $
    [ put . accessFlags
    , putConstantRef . nameIndex
    , putConstantRef . descriptorIndex
    , putVector . attributes
    ] <*> [ method ]
