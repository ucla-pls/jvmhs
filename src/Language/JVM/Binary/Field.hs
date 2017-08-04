module Language.JVM.Binary.Field
  ( Field (..)
  ) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.List                     (foldl')
import qualified Data.Set                      as S
import qualified Data.Vector                   as V

import           Language.JVM.Binary.Attribute (Attribute)
import           Language.JVM.Binary.Constant  (ConstantRef, getConstantRef,
                                                putConstantRef)
import           Language.JVM.Binary.Helpers

data Field = Field
  { accessFlags     :: AccessFlags
  , nameIndex       :: ConstantRef
  , discriptorIndex :: ConstantRef
  , attributes      :: V.Vector Attribute
  } deriving (Show, Eq)

instance Binary Field where
  get = Field
    <$> get
    <*> getConstantRef
    <*> getConstantRef
    <*> getVector

  put field = sequence_ $
    [ put . accessFlags
    , putConstantRef . nameIndex
    , putConstantRef . discriptorIndex
    , putVector . attributes
    ] <*>  [ field ]

data AccessFlag
  = Public
  | Private
  | Protected
  | Static
  | Final
  | Synchronized
  | Unused6
  | Volatile
  | Transient
  | Unused9
  | Unused10
  | Unused11
  | Unused12
  | Synthetic
  | Unused14
  | Enum
  deriving (Ord, Show, Eq, Enum)


newtype AccessFlags = AccessFlags (S.Set AccessFlag)
  deriving (Ord, Show, Eq)

instance Binary AccessFlags where
  get = do
    word <- getWord16be
    return . AccessFlags $ S.fromList [ toEnum x | x <- [0..15], testBit word x ]

  put (AccessFlags f) = do
    let word = foldl' setBit zeroBits (map fromEnum $ S.toList f)
    putWord16be word
