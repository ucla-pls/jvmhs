module Language.JVM.Binary.Method
  ( Method (..)
  , AccessFlags (..)
  , AccessFlag (..)
  ) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.List                      (foldl')
import qualified Data.Set                       as S
import qualified Data.Vector                    as V

import           Language.JVM.Binary.Attribute  (Attribute)
import           Language.JVM.Binary.Constant   (ConstantRef, getConstantRef,
                                                 putConstantRef)
import           Language.JVM.Binary.Helpers

data Method = Method
  { accessFlags     :: AccessFlags
  , nameIndex       :: ConstantRef
  , descriptorIndex :: ConstantRef
  , attributes      :: V.Vector Attribute
  } deriving (Show, Eq)

instance Binary Method where
  get = Method
    <$> get
    <*> getConstantRef
    <*> getConstantRef
    <*> getVector

  put method = sequence_ $
    [ put . accessFlags
    , putConstantRef . nameIndex
    , putConstantRef . descriptorIndex
    , putVector . attributes
    ] <*> [ method ]

data AccessFlag
  = Public
  | Private
  | Protected
  | Static
  | Final
  | Synchronized
  | Bridge
  | Varargs
  | Native
  | Unused10
  | Abstract
  | StrictFP
  | Synthetic
  | Unused14
  | Unused15
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
