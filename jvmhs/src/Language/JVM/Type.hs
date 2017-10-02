{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.JVM.Type
  ( Type (..)
  , MethodDescriptor (..)
  , FieldDescriptor (..)

  , typeFromText
  , methodDescriptorFromText
  , fieldDescriptorFromText
  , writeMethodDesciptor
  ) where

import Text.Parser

import Prelude hiding (fail)

import qualified Data.Text as Text

import Data.Aeson.TH

import Language.JVM.ClassName

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

writeType :: Type -> Text.Text
writeType t = do
  case t of
    Byte -> "B"
    Char -> "C"
    Double -> "D"
    Float -> "F"
    Int -> "I"
    Long -> "J"
    Class (ClassName txt) ->
      mconcat [ "L" , txt, ";"]
    Short -> "S"
    Boolean ->"Z"
    Array st -> mconcat ["[", writeType st]

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

writeMethodDesciptor :: MethodDescriptor -> Text.Text
writeMethodDesciptor md =
  mconcat
    [ "("
    , mconcat (map writeType (argumentTypes md))
    , ")"
    , case (returnType md) of
       Just t -> writeType t
       Nothing -> "V"
    ]

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
