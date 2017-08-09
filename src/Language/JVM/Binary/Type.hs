{-# LANGUAGE TemplateHaskell #-}
module Language.JVM.Binary.Type
  ( Type (..)
  , ClassName (..)
  , MethodDescriptor (..)
  , FieldDescriptor (..)

  , typeFromText
  , methodDescriptorFromText
  , fieldDescriptorFromText
  ) where

import Text.Parser

import Prelude hiding (fail)

import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON, withText)
import Data.Aeson.TH

newtype ClassName =
  ClassName Text.Text
  deriving (Eq, Show, Ord)

instance FromJSON ClassName where
  parseJSON = withText "the classname" $ \text ->
    return $ ClassName text

instance ToJSON ClassName where
  toJSON (ClassName text) =
    toJSON text

data Type
  = Byte
  | Char
  | Double
  | Float
  | Int
  | Long
  | Class ClassName
  | Short
  | Boolean
  | Array Type
  deriving (Eq, Show, Ord)

readType :: Parser Type
readType = do
  char <- readChar
  case char of
    'B' -> return Byte
    'C' -> return Char
    'D' -> return Double
    'F' -> return Float
    'I' -> return Int
    'J' -> return Long
    'L' -> do
      txt <- readWhile (/= ';')
      next ';'
      return $ Class (ClassName txt)
    'S' -> return Short
    'Z' -> return Boolean
    '[' -> Array <$> readType
    _ -> fail $ "Unknown char " ++ show char

typeFromText :: Text.Text -> Either String Type
typeFromText = parseAll readType

newtype FieldDescriptor =
  FieldDescriptor Type
  deriving (Show, Eq, Ord)

readFieldDescriptor :: Parser FieldDescriptor
readFieldDescriptor =
  FieldDescriptor <$> readType

fieldDescriptorFromText :: Text.Text -> Either String FieldDescriptor
fieldDescriptorFromText = parseAll readFieldDescriptor

data MethodDescriptor = MethodDescriptor
  { argumentTypes :: [ Type ]
  , returnType :: Maybe Type
  } deriving (Show, Eq, Ord)

readMethodDesciptor :: Parser MethodDescriptor
readMethodDesciptor = do
  next '('
  args <- readStar readType
  next ')'
  rtype <- (Just <$> readType <|> do { next 'V'; return Nothing })
  return $ MethodDescriptor args rtype

methodDescriptorFromText :: Text.Text -> Either String MethodDescriptor
methodDescriptorFromText = parseAll readMethodDesciptor

concat <$> mapM
  (deriveJSON
    (defaultOptions {
        sumEncoding = TaggedObject
                      { tagFieldName      = "type"
                      , contentsFieldName = "value"
                      }})
  ) [
  ''Type, ''FieldDescriptor, ''MethodDescriptor
  ]
