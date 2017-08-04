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

-- type ConstantValue = ConstantRef

-- data Code = Code
--   { maxStack :: Int16
--   , maxLocals :: Int16
--   -- codeLength :: Int32
--   , code :: ByteCode
--   -- exceptionTableLength :: Int16
--   , exceptionTable :: V.Vector Exception
--   , attributes :: V.Vector Attribute
--   } deriving (Show, Eq)
