{-# LANGUAGE TemplateHaskell #-}
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

import           Language.JVM.Binary.Attribute  (Attribute)
import           Language.JVM.Binary.Constant   (ConstantRef)
import           Language.JVM.Binary.SizedList

import Data.Aeson
import Data.Aeson.TH

data Method = Method
  { accessFlags     :: AccessFlags
  , nameIndex       :: ConstantRef
  , descriptorIndex :: ConstantRef
  , attributes      :: SizedList16 Attribute
  } deriving (Show, Eq)

instance Binary Method where
  get = Method
    <$> get
    <*> get
    <*> get
    <*> get

  put method = sequence_ $
    [ put . accessFlags
    , put . nameIndex
    , put . descriptorIndex
    , put . attributes
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
  | Unused16
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

deriveJSON defaultOptions ''AccessFlag
deriveJSON defaultOptions ''AccessFlags
