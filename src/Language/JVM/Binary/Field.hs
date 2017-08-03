module Language.JVM.Binary.Field
  ( Field (..)
  ) where

import Data.Word
import qualified Data.Vector as V

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put ()

import Language.JVM.Binary.Attribute (Attribute)
import Language.JVM.Binary.Constant (ConstantRef, getConstantRef, putConstantRef)
import Language.JVM.Binary.Helpers

data Field = Field
  { accessFlags :: !Word16
  , nameIndex :: ConstantRef
  , discriptorIndex :: ConstantRef
  , attributes :: V.Vector Attribute
  } deriving (Show, Eq)

instance Binary Field where
  get =
    Field <$> getWord16be
    <*> getConstantRef
    <*> getConstantRef
    <*> getVector

  put field = sequence_ $
    [ put . accessFlags
    , putConstantRef . nameIndex
    , putConstantRef . discriptorIndex
    , putVector . attributes
    ] <*>  [ field ]
