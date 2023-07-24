{-# LANGUAGE BangPatterns #-}
{-# HLINT ignore "Use record patterns" #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
Module      : Jvmhs.Data.Code
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD3
Maintainer  : kalhuage@cs.ucla.edu

This module works with the Code. This is a work in progress.
-}
module Jvmhs.Data.Code (
  Code (..),
  codeMaxStack,
  codeMaxLocals,
  codeByteCode,
  codeExceptionTable,
  codeStackMap,
  ExceptionHandler (..),
  ehStart,
  ehEnd,
  ehHandler,
  ehCatchType,
  verificationTypeInfo,

  -- * ByteCodeInst
  ByteCodeInst,
  byteCodeOffset,
  byteCodeOpcode,

  -- * Re-exports
  B.StackMapTable (..),
  B.StackMapFrame (..),
  B.StackMapFrameType (..),
  B.VerificationTypeInfo (..),
) where

-- base
import Data.Word
import GHC.Generics (Generic)

-- nfdata
import Control.DeepSeq

-- lens
import Control.Lens hiding ((.=))

-- aeson
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types

-- text
import qualified Data.Text as Text
import qualified Data.Vector as V

-- jvm-binary
import qualified Language.JVM as B
import qualified Language.JVM.Attribute.StackMapTable as B

import Jvmhs.Data.Identifier
import Jvmhs.Data.Type

type ByteCodeInst = B.ByteCodeInst B.High

-- type LineNumberTable = B.LineNumberTable B.High
type StackMapTable = B.StackMapTable B.High
type VerificationTypeInfo = B.VerificationTypeInfo B.High

data Code = Code
  { _codeMaxStack :: !Word16
  , _codeMaxLocals :: !Word16
  , _codeByteCode :: !(V.Vector ByteCodeInst)
  , _codeExceptionTable :: ![ExceptionHandler]
  , _codeStackMap :: !(Maybe StackMapTable)
  }
  deriving (Show, Eq, Generic, NFData)

data ExceptionHandler = ExceptionHandler
  { _ehStart :: !Int
  , _ehEnd :: !Int
  , _ehHandler :: !Int
  , _ehCatchType :: !(Maybe ClassName)
  }
  deriving (Show, Eq, Generic, NFData)

makeLenses ''Code
makeLenses ''ExceptionHandler

byteCodeOffset :: Lens' ByteCodeInst B.ByteCodeOffset
byteCodeOffset = lens B.offset (\a b -> a{B.offset = b})

byteCodeOpcode :: Lens' ByteCodeInst (B.ByteCodeOpr B.High)
byteCodeOpcode = lens B.opcode (\a b -> a{B.opcode = b})

verificationTypeInfo :: Traversal' StackMapTable VerificationTypeInfo
verificationTypeInfo g (B.StackMapTable s) =
  B.StackMapTable <$> (traverse . ver) g s
 where
  ver f (B.StackMapFrame fs ft) =
    B.StackMapFrame fs <$> case ft of
      B.SameLocals1StackItemFrame v -> B.SameLocals1StackItemFrame <$> f v
      B.AppendFrame v -> B.AppendFrame <$> traverse f v
      B.FullFrame v1 v2 -> B.FullFrame <$> traverse f v1 <*> traverse f v2
      _ -> pure ft

instance FromJSON ByteCodeInst where
  parseJSON v = fail "Not yet implemented"

instance ToJSON ByteCodeInst where
  toJSON bopr =
    object $
      ("opc" .= byteCodeOprOpCode (B.opcode bopr))
        : ("off" .= B.offset bopr)
        : case B.opcode bopr of
          B.ArrayLoad arrType -> ["type" .= fromArrayType arrType]
          B.ArrayStore arrType -> ["type" .= fromArrayType arrType]
          B.Push bconstant -> getListOfPairsFromBConstant bconstant
          B.Load localtype localaddr ->
            ["type" .= fromLocalType localtype, "addr" .= localaddr]
          B.Store localtype localaddr ->
            ["type" .= fromLocalType localtype, "addr" .= localaddr]
          B.BinaryOpr binopr arithmetictype ->
            ["opr" .= binopr, "type" .= fromArithmeticType arithmetictype]
          B.Neg arithmetictype -> ["type" .= fromArithmeticType arithmetictype]
          B.BitOpr bitopr wordsize -> ["opr" .= bitopr, "size" .= wordsize]
          B.IncrLocal localaddr increment ->
            ["addr" .= localaddr, "incr" .= increment]
          B.Cast castopr -> ["opr" .= castopr]
          B.CompareLongs -> []
          B.CompareFloating bool wordsize -> ["gt" .= bool, "size" .= wordsize]
          B.If cmpOpr oneOrTwo shortRelativeRef ->
            [ "opr" .= cmpOpr
            , "operands" .= oneOrTwo
            , "offset" .= shortRelativeRef
            ]
          B.IfRef bool oneOrTwo shortRelativeRef ->
            [ "equal" .= bool
            , "operands" .= oneOrTwo
            , "offset" .= shortRelativeRef
            ]
          B.Goto longrelativeref -> ["target" .= longrelativeref]
          B.Jsr longrelativeref -> ["target" .= longrelativeref]
          B.Ret localaddr -> ["addr" .= localaddr]
          B.TableSwitch longrelativeref switchtable ->
            ["default" .= longrelativeref, "table" .= switchtable]
          B.LookupSwitch longrelativeref vector ->
            ["default" .= longrelativeref, "pairs" .= vector]
          B.Get fa (AbsFieldId (InClass a b)) ->
            ["static" .= fa, "class" .= a, "field" .= b]
          B.Put fa (AbsFieldId (B.InClass a b)) ->
            ["static" .= fa, "class" .= a, "field" .= b]
          B.Invoke invocation -> getInvocationAttributes invocation
          B.New ref -> ["type" .= ref]
          B.NewArray (B.NewArrayType depth ref) ->
            ["depth" .= depth, "arraytype" .= ref]
          B.ArrayLength -> []
          B.Throw -> []
          B.CheckCast ref -> ["class" .= ref]
          B.InstanceOf ref -> ["class" .= ref]
          B.Monitor enter -> ["enter" .= enter]
          B.Return localtype -> case localtype of
            Just a -> ["type" .= fromLocalType a]
            Nothing -> ["type" .= Null]
          B.Nop -> []
          B.Pop popsize -> ["size" .= popsize]
          B.Dup wordsize -> ["size" .= wordsize]
          B.DupX1 wordsize -> ["size" .= wordsize]
          B.DupX2 wordsize -> ["size" .= wordsize]
          B.Swap -> []

byteCodeOprOpCode :: B.ByteCodeOpr B.High -> Text.Text
byteCodeOprOpCode = \case
  B.ArrayLoad _ -> "array_load"
  B.ArrayStore _ -> "array_store"
  B.Push _ -> "push"
  B.Load _ _ -> "load"
  B.Store _ _ -> "store"
  B.BinaryOpr _ _ -> "bin_op"
  B.Neg _ -> "neg"
  B.BitOpr _ _ -> "bit_op"
  B.IncrLocal _ _ -> "incr"
  B.Cast _ -> "cast"
  B.CompareLongs -> "cmp_long"
  B.CompareFloating _ _ -> "cmp_float"
  B.If _ _ _ -> "if"
  B.IfRef _ _ _ -> "if_ref"
  B.Goto _ -> "goto"
  B.Jsr _ -> "jsr"
  B.Ret _ -> "ret"
  B.TableSwitch _ _ -> "table_switch"
  B.LookupSwitch _ _ -> "lookup_switch"
  B.Get _ _ -> "get"
  B.Put _ _ -> "put"
  B.Invoke _ -> "invoke"
  B.New _ -> "new"
  B.NewArray _ -> "new_array"
  B.ArrayLength -> "array_length"
  B.Throw -> "throw"
  B.CheckCast _ -> "check_cast"
  B.InstanceOf _ -> "instance_of"
  B.Monitor _ -> "monitor"
  B.Return _ -> "return"
  B.Nop -> "nop"
  B.Pop _ -> "pop"
  B.Dup _ -> "dup"
  B.DupX1 _ -> "dupx1"
  B.DupX2 _ -> "dupx2"
  B.Swap -> "swap"

instance ToJSON B.FieldAccess where
  toJSON =
    Bool . \case
      B.FldStatic -> True
      B.FldField -> False

instance ToJSON (B.StackMapTable B.High) where
  toJSON (B.StackMapTable a) = object ["table" .= a]

instance FromJSON (B.StackMapTable B.High) where
  parseJSON = withObject "StackMapTable" \o ->
    B.StackMapTable <$> o .: "table"

instance ToJSON (B.StackMapFrame B.High) where
  toJSON (B.StackMapFrame deltaoffset frametype) =
    object (("offset" .= deltaoffset) : getFrameDetails frametype)

instance FromJSON (B.StackMapFrame B.High) where
  parseJSON = withObject "StackMapFrame" \o ->
    B.StackMapFrame <$> o .: "offset" <*> fail "not-yet-implemented"

instance ToJSON (B.VerificationTypeInfo B.High) where
  toJSON = object . getTypeInfo

instance ToJSON B.BinOpr where
  toJSON =
    String . \case
      B.Add -> "add"
      B.Sub -> "sub"
      B.Mul -> "mul"
      B.Div -> "div"
      B.Rem -> "rem"

instance ToJSON B.BitOpr where
  toJSON =
    String . \case
      B.ShL -> "shl"
      B.ShR -> "shr"
      B.UShR -> "ushr"
      B.And -> "and"
      B.Or -> "or"
      B.XOr -> "xor"

instance ToJSON B.WordSize where
  toJSON = \case
    B.One -> Number 1
    B.Two -> Number 2

instance ToJSON B.CastOpr where
  toJSON = \case
    B.CastDown smallarithmetictype ->
      object
        [ "kind" .= String "down"
        , "to" .= fromSmallArithmeticType smallarithmetictype
        ]
    B.CastTo fromtype totype ->
      object
        [ "kind" .= String "to"
        , "from" .= fromArithmeticType fromtype
        , "to" .= fromArithmeticType totype
        ]

instance ToJSON B.CmpOpr where
  toJSON =
    String . \case
      B.CEq -> "eq"
      B.CNe -> "ne"
      B.CLt -> "lt"
      B.CGe -> "ge"
      B.CGt -> "gt"
      B.CLe -> "le"

instance ToJSON (B.SwitchTable B.High) where
  toJSON (B.SwitchTable switchLow switchOffsets) =
    object ["switch_low" .= switchLow, "offsets" .= switchOffsets]

getListOfPairsFromBConstant :: Maybe JValue -> [Pair]
getListOfPairsFromBConstant = \case
  Just (VInteger a) -> ["type" .= STInteger, "value" .= a]
  Just (VFloat a) -> ["type" .= STFloat, "value" .= a]
  Just (VDouble a) -> ["type" .= STDouble, "value" .= a]
  Just (VLong a) -> ["type" .= STLong, "value" .= a]
  Just (VString a) ->
    ["type" .= STRef, "class" .= String "java/lang/String", "value" .= a]
  Just (VClass a) ->
    ["type" .= STRef, "class" .= String "java/lang/Class", "value" .= a]
  Just (VMethodType a) ->
    [ "type" .= STRef
    , "class" .= String "java/lang/invoke/MethodType"
    , "value" .= a
    ]
  Just (VMethodHandle a) ->
    [ "type" .= STRef
    , "class" .= String "java/lang/invoke/MethodHandle"
    , "value" .= a
    ]
  Nothing ->
    ["type" .= STRef, "class" .= String "java/lang/Object", "value" .= Null]

getInvocationAttributes :: B.Invocation B.High -> [Pair]
getInvocationAttributes = \case
  B.InvkSpecial (B.AbsVariableMethodId b avmi) ->
    ("kind" .= String "special") : ("interface" .= b) : getInRefTypeMethod avmi
  B.InvkVirtual abs' -> ("kind" .= String "virtual") : getInRefTypeMethod abs'
  B.InvkStatic (B.AbsVariableMethodId b avmi) ->
    ("kind" .= String "static") : ("interface" .= b) : getInRefTypeMethod avmi
  B.InvkInterface count avmi ->
    (["kind" .= String "interface", "count" .= count] <> getAbsInterfaceMethodId avmi)
  B.InvkDynamic invokeDynamicMethod ->
    ("kind" .= String "dynamic") : getInvokeDynamicMethod invokeDynamicMethod

getAbsInterfaceMethodId :: B.AbsInterfaceMethodId -> [Pair]
getAbsInterfaceMethodId (B.AbsInterfaceMethodId interfaceMethodId) =
  getInRefTypeMethod interfaceMethodId

-- getInClassMethod ::AbsMethodId -> [Pair]
-- getInClassMethod (AbsMethodId (InClass a b)) =
--   [ "class" .= a
--   , "method" .= b
--   ]

getInRefTypeMethod :: InRefType MethodId -> [Pair]
getInRefTypeMethod (InRefType a b) = ["reftype" .= a, "method" .= b]

getInvokeDynamicMethod :: B.InvokeDynamic B.High -> [Pair]
getInvokeDynamicMethod = \case
  (B.InvokeDynamic attrIndex methodid) ->
    ["attr" .= attrIndex, "method" .= methodid]

-- * Type Conversion

data TypeName
  = STInteger
  | STLong
  | STShort
  | STChar
  | STByte
  | STBoolean
  | STFloat
  | STDouble
  | STRef
  deriving (Show, Eq, Enum)

instance ToJSON TypeName where
  toJSON =
    String . \case
      STInteger -> "I"
      STLong -> "J"
      STShort -> "S"
      STChar -> "C"
      STByte -> "B"
      STBoolean -> "Z"
      STFloat -> "F"
      STDouble -> "D"
      STRef -> "R"

fromSmallArithmeticType :: B.SmallArithmeticType -> TypeName
fromSmallArithmeticType = \case
  B.MByte -> STByte
  B.MChar -> STChar
  B.MShort -> STShort

fromArithmeticType :: B.ArithmeticType -> TypeName
fromArithmeticType = \case
  B.MInt -> STInteger
  B.MLong -> STLong
  B.MFloat -> STFloat
  B.MDouble -> STDouble

fromArrayType :: B.ArrayType -> TypeName
fromArrayType = \case
  B.AByte -> STByte
  B.AChar -> STChar
  B.AShort -> STShort
  B.AInt -> STInteger
  B.ALong -> STLong
  B.AFloat -> STFloat
  B.ADouble -> STDouble
  B.ARef -> STRef

fromLocalType :: B.LocalType -> TypeName
fromLocalType = \case
  B.LInt -> STInteger
  B.LLong -> STLong
  B.LFloat -> STFloat
  B.LDouble -> STDouble
  B.LRef -> STRef

-- * VerificationTypeInfo

getTypeInfo :: B.VerificationTypeInfo B.High -> [Pair]
getTypeInfo = \case
  B.VTTop -> ["type" .= String "T"]
  B.VTInteger -> ["type" .= String "I"]
  B.VTFloat -> ["type" .= String "F"]
  B.VTLong -> ["type" .= String "J"]
  B.VTDouble -> ["type" .= String "D"]
  B.VTNull -> ["type" .= String "null"]
  B.VTObject ref -> ["type" .= String "object", "class" .= ref]
  B.VTUninitialized offset ->
    ["type" .= String "uninitialized", "offset" .= offset]
  B.VTUninitializedThis -> ["type" .= String "uninitialized_this"]

getFrameDetails :: B.StackMapFrameType B.High -> [Pair]
getFrameDetails = \case
  B.SameFrame -> ["kind" .= String "same"]
  B.SameLocals1StackItemFrame typeinfo ->
    ("kind" .= String "same_locals_stack_item") : getTypeInfo typeinfo
  B.ChopFrame size -> ["kind" .= String "chop", "size" .= size]
  B.AppendFrame typeinfoList ->
    ["kind" .= String "append", "type_list" .= typeinfoList]
  B.FullFrame (B.SizedList a) (B.SizedList b) ->
    ["kind" .= String "full_frame", "type_list1" .= a, "type_list2" .= b]

-- $(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 3} ''ExceptionHandler)

-- $(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 5} ''Code)
