module Language.JVM.Binary.Attribute
  ( Attribute (..)
  ) where


import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.ByteString as BS

import           Language.JVM.Binary.Constant (ConstantRef, getConstantRef, putConstantRef)

data Attribute = Attribute
  { attributeNameIndex :: ConstantRef
  , info               :: BS.ByteString
  } deriving (Show, Eq)

instance Binary Attribute where
  get = do
    nameIndex <- getConstantRef
    len <- getWord32be
    Attribute nameIndex <$> getByteString (fromIntegral len)

  put attr = do
    putConstantRef (attributeNameIndex attr)
    putWord32be (fromIntegral . BS.length $ info attr)
    putByteString $ info attr

