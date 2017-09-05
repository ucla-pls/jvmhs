{-# LANGUAGE TemplateHaskell #-}
module Language.JVM.Binary.Attribute
  ( Attribute (..)
  , nameIndex
  , info
  , name
  ) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

import qualified Data.Text as T

import Control.Lens

import qualified Data.ByteString              as BS

import           Language.JVM.Binary.Constant (ConstantRef, ConstantPool, toText)


data Attribute = Attribute
  { _nameIndex :: ConstantRef
  , _info      :: BS.ByteString
  } deriving (Show, Eq)

makeLenses ''Attribute

name :: ConstantPool -> Getter Attribute (Maybe T.Text)
name cp = nameIndex . toText cp

instance Binary Attribute where
  get = do
    nameIndex' <- get
    len <- getWord32be
    Attribute nameIndex' <$> getByteString (fromIntegral len)

  put attr = do
    put (_nameIndex attr)
    putWord32be (fromIntegral . BS.length $ _info attr)
    putByteString $ _info attr
