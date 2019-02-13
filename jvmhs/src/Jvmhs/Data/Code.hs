{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
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
import           Control.Lens                         hiding ((.=))
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.Text                            as Text
import qualified Data.Vector                          as V
import           Data.Word

import           GHC.Generics                         (Generic)

import qualified Language.JVM                         as B
import qualified Language.JVM.Attribute.Code          as B
import qualified Language.JVM.Attribute.StackMapTable as B
-- import qualified Language.JVM.Constant                as B
-- import qualified Language.JVM.Stage                   as B
-- import qualified Language.JVM.Type                    as B
-- import qualified Language.JVM.Utils                   as B

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
  toJSON bopr =
    object $ ("opc" .= byteCodeOprOpCode bopr) : case bopr of

    B.ArrayLoad arrType ->
      [ "type" .= arrType
      ]

    B.ArrayStore arrType ->
      [ "type" .= arrType
      ]

    B.Push bconstant ->
      getListOfPairsFromBConstant bconstant

    B.Load localtype localaddr ->
      [ "type" .= localtype
      , "addr" .= localaddr
      ]

    B.Store localtype localaddr ->
      [ "type" .= localtype
      , "addr" .= localaddr
      ]

    B.BinaryOpr binopr arithmetictype ->
      [ "opr" .= binopr
      , "type" .= arithmetictype
      ]

    B.Neg arithmetictype ->
      [ "type" .= arithmetictype
      ]

    B.BitOpr bitopr wordsize ->
      [ "opr" .= bitopr
      , "size" .= wordsize
      ]

    B.IncrLocal localaddr increment ->
      [ "addr" .= localaddr
      , "incr" .= increment
      ]

    B.Cast castopr ->
      [ "opr" .= castopr
      ]

    B.CompareLongs -> []

    B.CompareFloating bool wordsize ->
      [ "gt" .= bool
      , "size" .= wordsize
      ]

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

    B.Goto longrelativeref ->
      [ "ref" .= longrelativeref
      ]

    B.Jsr longrelativeref ->
      [ "ref" .= longrelativeref
      ]

    B.Ret localaddr ->
      [ "addr" .= localaddr
      ]

    B.TableSwitch longrelativeref switchtable ->
      [ "ref" .= longrelativeref
      , "table" .= switchtable
      ]

    B.LookupSwitch longrelativeref vector ->
      [ "ref" .= longrelativeref
      , "vector" .= vector
      ]

    B.Get fa (B.InClass a b) ->
      [ "static" .= fa
      , "class" .= B.classNameAsText a
      , "field" .= nameAndTypeFromFieldId b
      ]

    B.Put fa (B.InClass a b) ->
      [ "static" .= fa
      , "class" .= B.classNameAsText a
      , "field" .= nameAndTypeFromFieldId b
      ]

    B.Invoke invocation ->
      getInvocationAttributes invocation

    B.New ref ->
      [ "ref" .= ref
      ]

    B.NewArray arraytype ->
      getArrayType arraytype

    B.ArrayLength ->
      []

    B.Throw ->
      []

    B.CheckCast ref ->
      [ "ref" .= ref
      ]

    B.InstanceOf ref ->
      [ "ref" .= ref
      ]

    B.Monitor enter ->
      [ "enter" .= enter
      ]

    B.MultiNewArray ref word8 ->
      [ "ref" .= ref
      , "dimensions" .= word8
      ]

    B.Return lt ->
      [ "type" .= lt
      ]

    B.Nop -> []

    B.Pop popsize ->
      [ "size" .= popsize
      ]

    B.Dup wordsize ->
      [ "size" .= wordsize
      ]

    B.DupX1 wordsize ->
      [ "size" .= wordsize
      ]

    B.DupX2 wordsize ->
      [ "size" .= wordsize
      ]

    B.Swap ->
      []


byteCodeOprOpCode :: ByteCodeOpr -> Text.Text
byteCodeOprOpCode = \case
    B.ArrayLoad  _          -> "array_load"
    B.ArrayStore  _         -> "array_store"
    B.Push  _               -> "push"
    B.Load _ _              -> "load"
    B.Store  _  _           -> "store"
    B.BinaryOpr  _  _       -> "bin_op"
    B.Neg  _                -> "neg"
    B.BitOpr  _  _          -> "bit_op"
    B.IncrLocal  _  _       -> "incr"
    B.Cast  _               -> "cast"
    B.CompareLongs          -> "cmp_long"
    B.CompareFloating  _  _ -> "cmp_float"
    B.If  _  _  _           -> "if"
    B.IfRef  _  _  _        -> "if_ref"
    B.Goto  _               -> "goto"
    B.Jsr  _                -> "jsr"
    B.Ret  _                -> "ret"
    B.TableSwitch  _  _     -> "table_switch"
    B.LookupSwitch  _  _    -> "lookup_switch"
    B.Get  _ _              -> "get"
    B.Put  _ _              -> "put"
    B.Invoke  _             -> "invoke"
    B.New  _                -> "new"
    B.NewArray  _           -> "new_array"
    B.ArrayLength           -> "array_length"
    B.Throw                 -> "throw"
    B.CheckCast  _          -> "check_cast"
    B.InstanceOf  _         -> "instance_of"
    B.Monitor  _            -> "monitor"
    B.MultiNewArray  _  _   -> "multi_new_array"
    B.Return  _             -> "return"
    B.Nop                   -> "nop"
    B.Pop  _                -> "pop"
    B.Dup  _                -> "dup"
    B.DupX1  _              -> "dupx1"
    B.DupX2  _              -> "dupx2"
    B.Swap                  -> "swap"



instance ToJSON (B.LocalType) where
  toJSON = String . \case
    B.LInt -> getSimpleTypeString "integer"
    B.LLong -> getSimpleTypeString "long"
    B.LFloat -> getSimpleTypeString "float"
    B.LDouble -> getSimpleTypeString "double"
    B.LRef -> getSimpleTypeString "ref"

instance ToJSON (B.FieldAccess) where
  toJSON = Bool . \case
    B.FldStatic -> True
    B.FldField -> False



instance ToJSON (B.StackMapTable B.High) where
  toJSON (B.StackMapTable a) = object
      [ "table" .= a
      ]


instance ToJSON (B.StackMapFrame B.High) where
  toJSON (B.StackMapFrame deltaoffset frametype) = object
      (("offset" .= deltaoffset):getFrameDetails frametype)


instance ToJSON (B.VerificationTypeInfo B.High) where
  toJSON = object . getTypeInfo

instance ToJSON (B.ArrayType) where
  toJSON = String . \case
    B.AByte -> getSimpleTypeString "byte"
    B.AChar -> getSimpleTypeString "char"
    B.AShort -> getSimpleTypeString "short"
    B.AInt -> getSimpleTypeString "integer"
    B.ALong -> getSimpleTypeString "long"
    B.AFloat -> getSimpleTypeString "float"
    B.ADouble -> getSimpleTypeString "double"
    B.ARef -> getSimpleTypeString "ref"

instance ToJSON (B.BinOpr) where
  toJSON = String . \case
    B.Add -> "add"
    B.Sub -> "sub"
    B.Mul -> "mul"
    B.Div -> "div"
    B.Rem -> "rem"

instance ToJSON (B.ArithmeticType) where
  toJSON = String . \case
    B.MInt -> getSimpleTypeString "integer"
    B.MLong -> getSimpleTypeString "long"
    B.MFloat -> getSimpleTypeString "float"
    B.MDouble -> getSimpleTypeString "double"

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
    B.One -> 1
    B.Two -> 2

instance ToJSON (B.CastOpr) where
  toJSON = \case
    B.CastDown smallarithmetictype -> object
      [ "caste_type" .= String "down"
      , "to_type" .= smallarithmetictype
      ]
    B.CastTo fromtype totype -> object
      [ "cast_type" .= String "to"
      , "from_type" .= fromtype
      , "to_type" .= totype
      ]

instance ToJSON (B.SmallArithmeticType) where
  toJSON = String . \case
    B.MByte -> getSimpleTypeString "byte"
    B.MChar -> getSimpleTypeString "char"
    B.MShort -> getSimpleTypeString "short"

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
      [ "switch_low" .= switchLow
      , "offsets" .= switchOffsets
      ]


instance ToJSON (B.AbsVariableMethodId B.High) where
  toJSON = \case
    B.VInterfaceMethodId interface_method -> object
      [ "kind" .= String "interface_method"
      , "method" .= interface_method
      ]
    B.VMethodId (B.InClass a b) -> object
      [ "kind" .= String "method"
      , "class" .= B.classNameAsText a
      , "method" .= methodIDToText b
      ]

instance ToJSON (B.AbsInterfaceMethodId B.High) where
  toJSON interfacemethod = object
      [ "type" .= String "abs_interface_method"
      , "method" .= interfacemethod
      ]


instance ToJSON (B.InvokeDynamic B.High) where
  toJSON (B.InvokeDynamic attrIndex method) = object
      [ "attr_index" .= attrIndex
      , "method" .= method
      ]

instance ToJSON (B.MethodId) where
  toJSON (B.MethodId (B.NameAndType name args)) = object
      [ "signature" .= Text.concat [name,":",(B.toText args)]
      ]



getListOfPairsFromBConstant :: Maybe JValue -> [Pair]
getListOfPairsFromBConstant = \case
    Just (VInteger a) ->
      [ "type" .= getSimpleTypeString "integer"
      , "value" .= a
      ]
    Just (VFloat a) ->
      [ "type" .= getSimpleTypeString "float"
      , "value" .= a
      ]
    Just (VDouble a) ->
      [ "type" .= getSimpleTypeString "double"
      , "value" .= a
      ]
    Just (VString a) ->
      [ "type" .= getSimpleTypeString "string"
      , "value" .= a
      ]
    Just (VLong a) -> 
      [ "type" .= getSimpleTypeString "long" 
      , "value" .= a
      ]
    Just (VClass a) ->
      [ "type" .= getSimpleTypeString "class"
      , "value" .= a
      ]
    Just (VMethodType a) ->
      [ "type" .= getSimpleTypeString "method"
      , "value" .= a
      ]
    Just (VMethodHandle a) ->
      [ "type" .= getSimpleTypeString "method_handle"
      , "value" .= a
      ]
    Nothing ->
      [ "type" .= getSimpleTypeString "null"
      ]


methodIDToText :: B.MethodId -> Text.Text
methodIDToText (B.MethodId (B.NameAndType name args)) =
  Text.concat [name,":",(B.toText args)]

nameAndTypeFromFieldId :: B.FieldId -> Text.Text
nameAndTypeFromFieldId (B.FieldId (B.NameAndType name type_)) =
  Text.concat [name,":",B.toText type_]


getInClassMethod ::B.AbsMethodId B.High -> [Pair]
getInClassMethod (B.InClass a b) =
  [ "class" .= B.classNameAsText a
  , "method" .= methodIDToText b
  ]


getInvocationAttributes :: B.Invocation B.High -> [Pair]
getInvocationAttributes = \case
    B.InvkSpecial avmi ->
      ("kind" .= String "special")
      : getAbsVariableMethodId avmi

    B.InvkVirtual abs' ->
      ("kind" .= String "virtual")
      : getInClassMethod abs'

    B.InvkStatic avmi ->
      ("kind" .= String "static")
     : getAbsVariableMethodId avmi

    B.InvkInterface word8 avmi ->
       ["kind" .= String "interface"
       , "word8" .= word8
       ] ++ getAbsInterfaceMethodId avmi

    B.InvkDynamic invokeDynamicMethod ->
      ("kind" .= String "dynamic")
     : getInvokeDynamicMethod invokeDynamicMethod


getAbsInterfaceMethodId :: B.AbsInterfaceMethodId B.High -> [Pair]
getAbsInterfaceMethodId (B.AbsInterfaceMethodId interfaceMethodId) 
    = getInClassMethod interfaceMethodId


getAbsVariableMethodId :: B.AbsVariableMethodId B.High -> [Pair]
getAbsVariableMethodId = \case
  B.VInterfaceMethodId a ->
    [ "method" .= a
    ]
  B.VMethodId abs' ->
    getInClassMethod abs'

getInvokeDynamicMethod :: B.InvokeDynamic B.High -> [Pair]
getInvokeDynamicMethod = \case 
  (B.InvokeDynamic attrIndex methodid) ->
    [ "attr_index" .= attrIndex
    , "method" .= methodIDToText methodid
    ] 

getArrayType :: B.ExactArrayType B.High -> [Pair]
getArrayType = \case
    B.EABoolean -> ["type" .= getSimpleTypeString "boolean"]
    B.EAByte -> ["type" .= getSimpleTypeString "byte"]
    B.EAChar -> ["type" .= getSimpleTypeString "char"]
    B.EAShort -> ["type" .= getSimpleTypeString "short"]
    B.EAInt -> ["type" .= getSimpleTypeString "integer"]
    B.EALong -> ["type" .= getSimpleTypeString "long"]
    B.EAFloat -> ["type" .= getSimpleTypeString "float"]
    B.EADouble -> ["type" .= getSimpleTypeString "double"]
    B.EARef ref -> 
      ["type" .= getSimpleTypeString "ref"
      , "ref" .= ref
      ]

getTypeInfo :: B.VerificationTypeInfo B.High -> [Pair]
getTypeInfo = \case
  B.VTTop -> ["type" .= getSimpleTypeString "top"]
  B.VTInteger -> ["type" .= getSimpleTypeString "integer"]
  B.VTFloat -> ["type" .= getSimpleTypeString "float"]
  B.VTLong -> ["type" .= getSimpleTypeString "long"]
  B.VTDouble -> ["type" .= getSimpleTypeString "double"]
  B.VTNull -> ["type" .= getSimpleTypeString "null"]
  B.VTObject ref ->
    [ "type" .= getSimpleTypeString "object"
    , "ref" .= ref
    ]
  B.VTUninitialized word ->
    [ "type" .= getSimpleTypeString "uninitialized"
    , "word" .= word
    ]
  B.VTUninitializedThis  -> ["type" .= getSimpleTypeString "null"]


getSimpleTypeString :: Text.Text -> Text.Text
getSimpleTypeString = \case
      "integer" -> "I"
      "long" -> "J"
      "short" -> "S"
      "char" -> "C"
      "byte" -> "B"
      "boolean" -> "B"
      "float" -> "F"
      "double" -> "D"
      "ref" -> "["
      "string" -> "string"
      "class" -> "class"
      "method" -> "method"
      "method_handle" -> "method_handle"
      "null" -> "null"
      "top" -> "T"
      "uninitialized" -> "U"
      "object" -> "O"
      _ -> "<incorrect_code>"


getFrameDetails :: B.StackMapFrameType B.High -> [Pair]
getFrameDetails = \case
    B.SameFrame ->
      [ "kind" .= String "same"
      ]
    B.SameLocals1StackItemFrame typeinfo ->
      ("kind" .= String "same_locals_stack_item")
     : getTypeInfo typeinfo
    B.ChopFrame size ->
      [ "kind" .= String "chop"
      , "size" .= size
      ]
    B.AppendFrame typeinfoList ->
      [ "kind" .= String "append"
      , "type_list" .= typeinfoList
      ]

    B.FullFrame (B.SizedList a) (B.SizedList b) ->
      [ "kind" .= String "full_frame"
      , "type_list1" .= a
      , "type_list2" .= b
      ]
