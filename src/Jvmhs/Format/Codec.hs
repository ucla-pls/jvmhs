{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

{-

An autodocodec

-}
module Jvmhs.Format.Codec where

import Control.Lens

import Autodocodec
import qualified Autodocodec as A
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import Data.Void
import Jvmhs.Data.Code
import Jvmhs.Data.Identifier
import qualified Language.JVM as B

codecClassName :: JSONCodec ClassName
codecClassName =
  bimapCodec (pure . review fullyQualifiedName) (view fullyQualifiedName) textCodec
    <?> "A classname seperated by '/'"

-- codecByteCodeInst :: JSONCodec ByteCodeInst
-- codecByteCodeInst = do
--   disjointMatchChoicesCodec "ByteCodeInst" enc dec
--  where
--   enc = \case
--     a -> ("test", pureCodec ())
--   dec :: HashMap.HashMap Discriminator (Text.Text, ObjectCodec Void ByteCodeInst)
--   dec =
--     HashMap.fromList
--       [ ("test", ("Swap", pureCodec B.Swap))
--       ]

codecByteCodeInst :: JSONCodec ByteCodeInst
codecByteCodeInst =
  object "ByteCodeInst" $
    B.ByteCodeInst
      <$> requiredField' "offset"
      A..= view byteCodeOffset
      <*> codecByteCodeOpr
      A..= view byteCodeOpcode

codecByteCodeOpr :: ObjectCodec (B.ByteCodeOpr B.High) (B.ByteCodeOpr B.High)
codecByteCodeOpr = do
  discriminatedUnionCodec "opc" enc dec
 where
  enc :: B.ByteCodeOpr B.High -> (Discriminator, ObjectCodec (B.ByteCodeOpr B.High) ())
  enc = \case
    B.Swap -> ("swap", pureCodec ())
    B.DupX1 s -> ("dupx1", mapToEncoder s codecWordSize')
    B.DupX2 s -> ("dupx2", mapToEncoder s codecWordSize')
    B.Dup s -> ("dup", mapToEncoder s codecWordSize')
    a -> ("swap", pureCodec ())
  dec :: HashMap.HashMap Discriminator (Text.Text, ObjectCodec Void (B.ByteCodeOpr B.High))
  dec =
    HashMap.fromList
      [ ("swap", ("Swap", mapToDecoder (const B.Swap) (pureCodec ())))
      , ("dupx1", ("DupX1", mapToDecoder B.DupX1 codecWordSize'))
      , ("dupx2", ("DupX2", mapToDecoder B.DupX2 codecWordSize'))
      , ("dup", ("Dup", mapToDecoder B.Dup codecWordSize'))
      ]

  codecWordSize' = requiredFieldWith' "size" codecWordSize

codecWordSize :: JSONCodec B.WordSize
codecWordSize =
  dimapCodec
    ( \case
        1 -> B.One
        2 -> B.Two
        _ -> error "Unexpected"
    )
    ( \case
        B.One -> 1
        B.Two -> 2
    )
    $ scientificWithBoundsCodec (NumberBounds 1 2)

codecCmpOpr :: JSONCodec B.CmpOpr
codecCmpOpr = do
  stringConstCodec
    [ (B.CEq, "eq")
    , (B.CNe, "ne")
    , (B.CLt, "lt")
    , (B.CGe, "ge")
    , (B.CGt, "gt")
    , (B.CLe, "le")
    ]

codecTypeName :: JSONCodec TypeName
codecTypeName = do
  annotatedEnumCodec
    [ (STInteger, "I", "An Integer (4 bytes)")
    , (STLong, "J", "A Long (8 bytes)")
    , (STShort, "S", "A Short (2 bytes)")
    , (STChar, "C", "A Char (1 byte)")
    , (STByte, "B", "A Byte (1 byte, unsigned)")
    , (STBoolean, "Z", "A Boolean (1 byte)")
    , (STFloat, "F", "A Float (4 bytes)")
    , (STDouble, "D", "A Double (4 bytes)")
    , (STRef, "R", "A Reference (4 bytes)")
    ]

annotatedEnumCodec
  :: Eq e
  => NE.NonEmpty (e, Text.Text, Text.Text)
  -> JSONCodec e
annotatedEnumCodec =
  enumCodec
    . fmap
      ( \(constant, text, ann) ->
          ( constant
          , literalTextValueCodec constant text <?> ann
          )
      )
