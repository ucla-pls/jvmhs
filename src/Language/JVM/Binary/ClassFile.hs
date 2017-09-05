{-# LANGUAGE DeriveGeneric #-}
-- Information from http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4
-- And from https://en.wikipedia.org/wiki/Java_class_file#cite_note-jvms-4.4-4

module Language.JVM.Binary.ClassFile
  ( ClassFile (..)
  , AccessFlags (..)
  , AccessFlag (..)

  , decodeClassFile
  , decodeClassFileOrFail
  ) where


import           GHC.Generics (Generic)
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.List                     (foldl')
import qualified Data.Set                      as S

import qualified Data.ByteString.Lazy          as BL

import           Language.JVM.Binary.Attribute (Attribute)
import           Language.JVM.Binary.Constant  (ConstantPool, ConstantRef)
import           Language.JVM.Binary.Field     (Field)
import           Language.JVM.Binary.SizedList
import           Language.JVM.Binary.Method    (Method)

data ClassFile = ClassFile
  { magicNumber  :: !Word32

  , minorVersion :: !Word16
  , majorVersion :: !Word16

  , constantPool :: ConstantPool

  , accessFlags  :: AccessFlags

  , thisClass    :: ConstantRef
  , superClass   :: ConstantRef

  , interfaces   :: SizedList16 ConstantRef
  , fields       :: SizedList16 Field
  , methods      :: SizedList16 Method
  , attributes   :: SizedList16 Attribute
  } deriving (Show, Eq, Generic)

instance Binary ClassFile where

decodeClassFile :: BL.ByteString -> ClassFile
decodeClassFile = decode

decodeClassFileOrFail :: BL.ByteString -> Either String ClassFile
decodeClassFileOrFail bs = do
  case decodeOrFail bs of
    Right (_, _, cf) -> Right cf
    Left (_, _, msg) -> Left msg

data AccessFlag
  = Public
  | Unused1
  | Unused2
  | Unused3
  | Final
  | Super
  | Unused6
  | Unused7
  | Unused8
  | Unused9
  | Abstract
  | Unused11
  | Synthetic
  | Annotation
  | Enum
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
