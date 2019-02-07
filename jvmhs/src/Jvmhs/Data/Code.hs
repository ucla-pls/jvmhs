{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Jvmhs.Data.Code
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module works with the Code. This is a work in progress.
-}

module Jvmhs.Data.Code
  ( Code (..)
  , codeMaxStack
  , codeMaxLocals
  , codeByteCode
  , codeExceptionTable
  , codeStackMap

  , traverseCode

  , fromBinaryCode
  , toBinaryCode

  , ExceptionHandler (..)
  , ehStart
  , ehEnd
  , ehHandler
  , ehCatchType

  , traverseExceptionHandler

  , StackMapTable
  , verificationTypeInfo
  , VerificationTypeInfo

  , ByteCodeOpr
  )
  where

import           Control.DeepSeq
import           Control.Lens hiding ((.=))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH
import           Data.Word
import qualified Data.Text as Text
import qualified Data.Vector as V

import           GHC.Generics (Generic)

import qualified Language.JVM                         as B
import qualified Language.JVM.Attribute.Code          as B
import qualified Language.JVM.Attribute.StackMapTable as B
import qualified Language.JVM.Constant                as B
import qualified Language.JVM.Type                    as B
import qualified Language.JVM.Utils                   as B
import qualified Language.JVM.Stage                   as B

import           Jvmhs.Data.Type

type ByteCodeOpr = B.ByteCodeOpr B.High
-- type LineNumberTable = B.LineNumberTable B.High
type StackMapTable = B.StackMapTable B.High
type VerificationTypeInfo = B.VerificationTypeInfo B.High

data Code = Code
  { _codeMaxStack       :: !Word16
  , _codeMaxLocals      :: !Word16
  , _codeByteCode       :: !(V.Vector ByteCodeOpr)
  , _codeExceptionTable :: ![ExceptionHandler]
  , _codeStackMap       :: !(Maybe StackMapTable)
  } deriving (Show, Eq, Generic, NFData)

data ExceptionHandler = ExceptionHandler
  { _ehStart     :: !Int
  , _ehEnd       :: !Int
  , _ehHandler   :: !Int
  , _ehCatchType :: !(Maybe ClassName)
  } deriving (Show, Eq, Generic, NFData)

makeLenses ''Code
makeLenses ''ExceptionHandler

fromBinaryCode :: B.Code B.High -> Code
fromBinaryCode =
  Code
    <$> B.codeMaxStack
    <*> B.codeMaxLocals
    <*> B.codeByteCodeOprs
    <*> fmap (view $ from _Binary) . B.unSizedList . B.codeExceptionTable
    <*> B.codeStackMapTable

toBinaryCode :: Code -> B.Code B.High
toBinaryCode c =
  B.Code
   (c^.codeMaxStack)
   (c^.codeMaxLocals)
   (B.ByteCode $ c^.codeByteCode)
   (B.SizedList $ c^..codeExceptionTable.folded._Binary)
   (B.CodeAttributes (maybe [] (:[]) $ c^.codeStackMap) [] [])

instance FromJVMBinary (B.ExceptionTable B.High) ExceptionHandler where
  _Binary = iso toBinaryExceptionTable fromBinaryExceptionTable
    where
      fromBinaryExceptionTable =
        ExceptionHandler
        <$> B.start
        <*> B.end
        <*> B.handler
        <*> fmap (view $ from _Binary) . B.catchType

      toBinaryExceptionTable =
        B.ExceptionTable
        <$> _ehStart
        <*> _ehEnd
        <*> _ehHandler
        <*> fmap (view _Binary) . _ehCatchType

traverseCode ::
     (Traversal' Word16 a)
  -> (Traversal' Word16 a)
  -> (Traversal' (V.Vector ByteCodeOpr) a)
  -> (Traversal' [ExceptionHandler] a)
  -> (Traversal' (Maybe StackMapTable) a)
  -> Traversal' Code a
traverseCode tms tml tbc tet tst g (Code ms ml bc et st) =
    Code
    <$> tms g ms
    <*> tml g ml
    <*> tbc g bc
    <*> tet g et
    <*> tst g st
{-# INLINE traverseCode #-}

traverseExceptionHandler ::
     Traversal' Int a
  -> Traversal' Int a
  -> Traversal' Int a
  -> Traversal' (Maybe ClassName) a
  -> Traversal' ExceptionHandler a
traverseExceptionHandler ts te th tc g (ExceptionHandler s e h c) =
  ExceptionHandler <$> ts g s <*> te g e <*> th g h <*> tc g c
{-# INLINE traverseExceptionHandler #-}

verificationTypeInfo :: Traversal' StackMapTable VerificationTypeInfo
verificationTypeInfo g (B.StackMapTable s) =
  B.StackMapTable <$> (traverse . ver) g s
  where
    ver f (B.StackMapFrame fs ft) =
      B.StackMapFrame fs <$>
      case ft of
        B.SameLocals1StackItemFrame v -> B.SameLocals1StackItemFrame <$> f v
        B.AppendFrame v -> B.AppendFrame <$> traverse f v
        B.FullFrame v1 v2 -> B.FullFrame <$> traverse f v1 <*> traverse f v2
        _ -> pure ft

$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 5} ''Code)
$(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 3} ''ExceptionHandler)

instance ToJSON ByteCodeOpr where
  toJSON = \case
    B.ArrayLoad arrType -> object
      [ "opc" .= String "array_load"
      , "type" .= toJSON arrType
      ]
    B.ArrayStore arrType -> object
      [ "opc" .= String "array_store"
      , "type" .= toJSON arrType
      ]
    B.Push bconstant -> object
      ([ "opc" .= String "push"] ++ getListOfPairsFromBConstant bconstant)
    B.Load localtype localaddr -> object
      [ "opc" .= String "load"
      , "type" .= toJSON localtype
      , "addr" .= toJSON localaddr
      ]
    B.Store localtype localaddr -> object
      [ "opc" .= String "store"
      , "type" .= toJSON localtype
      , "addr" .= toJSON localaddr
      ]
    B.BinaryOpr binopr arithmetictype -> object
      [ "opc" .= String "bin_op"
      , "opr" .= toJSON binopr
      , "type" .= toJSON arithmetictype
      ]
    B.Neg arithmetictype -> object
      [ "opc" .= String "neg"
      , "type" .= toJSON arithmetictype
      ]
    B.BitOpr bitopr wordsize -> object
      [ "opc" .= String "bit_op"
      , "opr" .= toJSON bitopr
      , "size" .= toJSON wordsize
      ]
    B.IncrLocal localaddr increment -> object
      [ "opc" .= String "incr"
      , "addr" .= toJSON localaddr
      , "incr" .= toJSON increment
      ]
    B.Cast castopr -> object
      [ "opc" .= String "cast"
      , "opr" .= toJSON castopr
      ]
    B.CompareLongs -> object
      [ "opc" .= String "cmp_long"]
    B.CompareFloating bool wordsize -> object
      [ "opc" .= String "cmp_float"
      , "gt" .= toJSON bool
      , "size" .= toJSON wordsize
      ]
    B.If cmpOpr oneOrTwo shortRelativeRef -> object
      [ "opc" .= String "if"
      , "opr" .= toJSON cmpOpr
      , "cmp_with_0" .= if oneOrTwo==B.One then (Bool True) else (Bool False)
      , "offset" .= toJSON shortRelativeRef --convertShortRelativeRefToJSON shortRelativeRef 
      ]
    B.IfRef bool oneOrTwo shortRelativeRef -> object
      [ "opc" .= String "if_ref"
      , "equal" .= toJSON bool
      , "one_or_two" .= toJSON oneOrTwo
      , "offset" .= toJSON shortRelativeRef
      ]
    B.Goto longrelativeref -> object
      [ "opc" .= String "goto"
      , "ref" .= toJSON longrelativeref
      ]
    B.Jsr longrelativeref -> object
      [ "opc" .= String "jsr"
      , "ref" .= toJSON longrelativeref
      ]
    B.Ret localaddr -> object
      [ "opc" .= String "ret"
      , "addr" .= toJSON localaddr
      ] 
    B.TableSwitch longrelativeref switchtable -> object
      [ "opc" .= String "table_switch"
      , "ref" .= toJSON longrelativeref
      , "table" .= toJSON switchtable
      ]  
    B.LookupSwitch longrelativeref vector -> object
      [ "opc" .= String "lookup_switch"
      , "ref" .= toJSON longrelativeref
      , "vector" .= toJSON vector
      ] 
    B.Get fa (B.InClass a b) -> object
      [ "opc" .= String "get"
      , "static" .= toJSON fa
      , "class" .= B.classNameAsText a
      , "name_and_type" .= nameAndTypeFromFieldId b
      ]

    B.Put fa (B.InClass a b) -> object
      [ "opc" .= String "put"
      , "static" .= toJSON fa
      , "class" .= B.classNameAsText a
      , "name_and_type" .= nameAndTypeFromFieldId b
      ]
    B.Invoke invocation -> object ( [ "opc" .= String "invoke"] ++ getInvocationAttributes invocation )
    B.New ref -> object
      [ "opc" .= String "new"
      , "ref" .= toJSON ref
      ] 
    B.NewArray arraytype -> object
      [ "opc" .= String "new_array"
      , "type" .= toJSON arraytype
      ]
    B.ArrayLength -> object
      [ "opc" .= String "array_length"]
    B.Throw -> object
      [ "opc" .= String "throw"]
    B.CheckCast ref -> object
      [ "opc" .= String "check)cast"
      , "ref" .= toJSON ref
      ]    
    B.InstanceOf ref -> object
      [ "opc" .= String "instance_of"
      , "ref" .= toJSON ref
      ] 
    B.Monitor enter -> object
      [ "opc" .= String "monitor"
      , "enter" .= toJSON enter
      ] 
    B.MultiNewArray ref word8 -> object
      [ "opc" .= String "multi_new_array"
      , "ref" .= toJSON ref
      , "dimensions" .= show word8
      ]     
    B.Return lt -> object
      [ "opc" .= String "return"
      , "type" .= toJSON lt
      ] 
    B.Nop -> object
      [ "opc" .= String "nop"]
    B.Pop popsize -> object
      [ "opc" .= String "pop"
      , "size" .= toJSON popsize
      ] 
    B.Dup wordsize -> object
      [ "opc" .= String "dup"
      , "size" .= toJSON wordsize
      ]
    B.DupX1 wordsize -> object
      [ "opc" .= String "dupx1"
      , "size" .= toJSON wordsize
      ]
    B.DupX2 wordsize -> object
      [ "opc" .= String "dupx2"
      , "size" .= toJSON wordsize
      ]
    B.Swap -> object
      [ "opc" .= String "swap"]
    _ ->
      String "<code>"


instance ToJSON (B.LocalType) where
  toJSON = String . \case
    B.LInt -> "I"
    B.LLong -> "J"
    B.LFloat -> "F"
    B.LDouble -> "D"
    B.LRef -> "L"

instance ToJSON (B.FieldAccess) where
  toJSON = Bool . \case
    B.FldStatic -> True
    B.FldField -> False



instance ToJSON (B.StackMapTable B.High) where
  toJSON (B.StackMapTable a) = object
      [ "table" .= toJSON a
      ]


instance ToJSON (B.StackMapFrame B.High) where
  toJSON (B.StackMapFrame deltaoffset frametype) = object
      ([ "offset" .= toJSON deltaoffset] ++ getFrameDetails frametype)

instance ToJSON (B.VerificationTypeInfo B.High) where
  toJSON = \case
    B.VTTop -> String "T"
    B.VTInteger -> String "I"
    B.VTFloat -> String "F"
    B.VTLong -> String "J"
    B.VTDouble -> String "D"
    B.VTNull -> String "NULL"
    B.VTObject ref -> object
      [ "type" .= String "O"
      , "ref" .= toJSON ref
      ]
    B.VTUninitialized word -> object
      [ "type" .= String "U"
      , "word" .= toJSON word
      ]

instance ToJSON (B.ArrayType) where
  toJSON = String . \case
    B.AByte -> "B"
    B.AChar -> "C"
    B.AShort -> "S"
    B.AInt -> "I"
    B.ALong -> "J"
    B.AFloat -> "F"
    B.ADouble -> "D"
    B.ARef -> "["

instance ToJSON (B.BinOpr) where
  toJSON = String . \case
    B.Add -> "add"
    B.Sub -> "sub"
    B.Mul -> "mul"
    B.Div -> "div"
    B.Rem -> "rem"

instance ToJSON (B.ArithmeticType) where
  toJSON = String . \case
    B.MInt -> "I"
    B.MLong -> "J"
    B.MFloat -> "F"
    B.MDouble -> "D"

instance ToJSON (B.BitOpr) where
  toJSON = String . \case
    B.ShL -> "shl"
    B.ShR -> "shr"
    B.UShR -> "ushr"
    B.And -> "and"
    B.Or -> "or"
    B.XOr -> "xor"

instance ToJSON (B.WordSize) where
  toJSON = String . \case
    B.One -> "1"
    B.Two -> "2"

instance ToJSON (B.CastOpr) where
  toJSON = \case
    B.CastDown smallarithmetictype -> object
      [ "caste_type" .= String "cast_down"
      , "to_type" .= toJSON smallarithmetictype
      ]
    B.CastTo fromtype totype -> object
      [ "cast_type" .= String "cast-to"
      , "from_type" .= toJSON fromtype
      , "to_type" .= toJSON totype
      ]

instance ToJSON (B.SmallArithmeticType) where
  toJSON = String . \case
    B.MByte -> "B"
    B.MChar -> "C"
    B.MShort -> "S"

instance ToJSON (B.CmpOpr) where
  toJSON = String . \case
    B.CEq -> "eq"
    B.CNe -> "ne"
    B.CLt -> "lt"
    B.CGe -> "ge"
    B.CGt -> "gt"
    B.CLe -> "le"

instance ToJSON (B.SwitchTable B.High) where
  toJSON (B.SwitchTable switchLow switchOffsets) = object
      [ "switch_low" .= show (switchLow)
      , "offsets" .= toJSON switchOffsets
      ]

instance ToJSON (B.ExactArrayType B.High) where
  toJSON = String . \case
    B.EABoolean -> "Z"
    B.EAByte -> "B"
    B.EAChar -> "C"
    B.EAShort -> "S"
    B.EAInt -> "I"
    B.EALong -> "J"
    B.EAFloat -> "F"
    B.EADouble -> "D"
    B.EARef ref -> "["


instance ToJSON (B.AbsVariableMethodId B.High) where
  toJSON = \case
    B.VInterfaceMethodId interface_method -> object
      [ "kind" .= String "interface_method"
      , "method" .= toJSON interface_method
      ]
    B.VMethodId (B.InClass a b) -> object
      [ "kind" .= String "method"
      , "class" .= B.classNameAsText a
      , "method" .= methodIDToText b
      ]


instance ToJSON (B.AbsInterfaceMethodId B.High) where
  toJSON interfacemethod = object
      [ 
        "type" .= String "abs_interface_method"
      , "method" .= toJSON interfacemethod
      ]


instance ToJSON (B.InvokeDynamic B.High) where
  toJSON (B.InvokeDynamic attrIndex method) = object
      [ 
        "attr_index" .= show (attrIndex)
      , "method" .= toJSON method
      ]

instance ToJSON (B.MethodId) where
  toJSON (B.MethodId (B.NameAndType name args)) = object
      [ 
       "signature" .= Text.concat [name,":",(B.toText args)]
      ]

methodIDToText :: B.MethodId -> Text.Text
methodIDToText (B.MethodId (B.NameAndType name args)) = Text.concat [name,":",(B.toText args)]


nameAndTypeFromFieldId :: B.FieldId -> Text.Text
nameAndTypeFromFieldId (B.FieldId (B.NameAndType name type_)) = Text.concat [name,":",B.toText type_] 


getListOfPairsFromBConstant = \case
    Just (VInteger a) ->
      [ "type" .= String "I"
      , "value" .= toJSON a
      ]
    Just (VFloat a) ->
      [ "type" .= String "F"
      , "value" .= toJSON a
      ]
    Just (VDouble a) ->
      [ "type" .= String "D"
      , "value" .= toJSON a
      ]
    Just (VString a) ->
      [ "type" .= String "string"
      , "value" .= toJSON a
      ]
    Just (VClass a) ->
      [ "type" .= String "class"
      , "value" .= toJSON a
      ]
    Just (VMethodType a) ->
      [ "type" .= String "method"
      , "value" .= toJSON a
      ]
    Just (VMethodHandle a) ->
      [ "type" .= String "method_handle"
      , "value" .= toJSON a
      ]
    Nothing ->
      [ "type" .= String "null"
      ]

getInvocationAttributes = \case
    B.InvkSpecial (B.VInterfaceMethodId a) ->
      [ "kind" .= String "special"
      , "ref" .= toJSON a
      ]
    B.InvkSpecial (B.VMethodId (B.InClass a b)) ->
      [ "kind" .= String "special" 
      , "class" .= B.classNameAsText a
      , "method" .= methodIDToText b
      ]
    B.InvkVirtual (B.InClass a b) ->
      [ "kind" .= String "virtual"
      , "class" .= B.classNameAsText a
      , "method" .= methodIDToText b
      ]  
    B.InvkStatic (B.VInterfaceMethodId a) ->
      [ "kind" .= String "static"
      , "ref" .= toJSON a
      ]
    B.InvkStatic (B.VMethodId (B.InClass a b)) ->
      [ "kind" .= String "static"
      , "class" .= B.classNameAsText a
      , "method" .= methodIDToText b
      ]
    B.InvkInterface word8 (B.AbsInterfaceMethodId (B.InClass a b))->
      [ "kind" .= String "interface"
      , "word8" .= toJSON word8
      , "class" .= B.classNameAsText a
      , "method" .= methodIDToText b
      ]
    B.InvkDynamic (B.InvokeDynamic index methodid) -> 
      [ "kind" .= String "dynamic"
      , "index" .= toJSON index
      , "method" .= methodIDToText methodid
      ] 
    _ ->
      ["idk" .= String "<code>"]


getFrameDetails = \case
    B.SameFrame -> 
      [ "kind" .= String "same"
      ]
    B.SameLocals1StackItemFrame typeinfo -> 
      [ "kind" .= String "same_locals_stack_item"
      , "type" .= toJSON typeinfo
      ]
    B.ChopFrame size -> 
      [ "kind" .= String "chop"
      , "size" .= toJSON size
      ]
    B.AppendFrame typeinfo -> 
      [ "kind" .= String "append"
      , "type" .= toJSON typeinfo
      ]
    B.FullFrame (B.SizedList a) (B.SizedList b) -> 
      [ "kind" .= String "full_frame"
      , "type_list1" .= toJSON a
      , "type_list2" .= toJSON b
      ]