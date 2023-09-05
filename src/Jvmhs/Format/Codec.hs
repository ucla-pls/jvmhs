{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-

An autodocodec

-}
module Jvmhs.Format.Codec where

-- class
import Jvmhs.Data.Class

-- conedec
import Conedec

import Data.Aeson (Value (..))
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import Jvmhs.Data.Code
import Jvmhs.Data.Type
import qualified Language.JVM as B
import Prelude hiding (all, any, null)

data V1

type HasV1 ctx =
  ( Def "ClassName" ValueCodec ctx ClassName
  , Def "TypeParameter" ValueCodec ctx (Annotated TypeParameter)
  , Def "Annotation" ValueCodec ctx Annotation
  , Def "ThrowsType" ValueCodec ctx (Annotated ThrowsType)
  , Def "ClassType" ValueCodec ctx (Annotated ClassType)
  , Def "Method" ValueCodec ctx Method
  , Def "Field" ValueCodec ctx Field
  , Def "ReturnType" ValueCodec ctx (Annotated ReturnType)
  )

instance Def "ClassName" ValueCodec V1 ClassName where unref = codecClassName

codecClassName :: Codec ValueCodec ctx ClassName
codecClassName =
  codecTextSerializeable
    <?> "a name of a class, packages seperated by '/'"
      <!> "java/util/Object"

instance Def "TypeParameter" ValueCodec V1 (Annotated TypeParameter) where unref = codecAnnotatedTypeParameter

codecAnnotatedTypeParameter :: HasV1 ctx => Codec ValueCodec ctx (Annotated TypeParameter)
codecAnnotatedTypeParameter =
  codecAnnotated
    ( all do
        #_typeParameterName ~ "name" <: simply text
        #_typeParameterClassBound
          ~ "classbound"
          <: optional (ref @"ThrowsType")
        #_typeParameterInterfaceBound
          ~ "interfacebound"
          <: manyOfList (ref @"ThrowsType")
    )

instance Def "ReturnType" ValueCodec V1 (Annotated ReturnType) where
  unref = broken

instance Def "JType" ValueCodec V1 JType where
  unref = any do
    #ifJTBase =: ref @"JBaseType"
    #ifJTRef =: ref @"JRefType"

instance Def "JBaseType" ValueCodec V1 JBaseType where
  unref = any do
    #ifJTByte =: "byte"
    #ifJTChar =: "char"
    #ifJTDouble =: "double"
    #ifJTFloat =: "float"
    #ifJTInt =: "int"
    #ifJTLong =: "long"
    #ifJTShort =: "short"
    #ifJTBoolean =: "boolean"

instance Def "JRefType" ValueCodec V1 JRefType where
  unref = object $ any do
    #ifJTClass ~ "class" <: ref @"ClassName"
    #ifJTArray ~ "array" <: ref @"JType"

instance Def "LocalType" ValueCodec V1 B.LocalType where
  unref = any do
    #ifLInt =: "int"
    #ifLLong =: "long"
    #ifLFloat =: "float"
    #ifLDouble =: "double"
    #ifLRef =: "ref"

instance Def "ArithmeticType" ValueCodec V1 B.ArithmeticType where
  unref = any do
    #ifMInt =: "int"
    #ifMLong =: "long"
    #ifMFloat =: "float"
    #ifMDouble =: "double"

instance Def "Parameter" ValueCodec V1 (Annotated Parameter) where
  unref = broken

instance Def "AnnotationValue" ValueCodec V1 AnnotationValue where
  unref = broken

instance Def "ClassType" ValueCodec V1 (Annotated ClassType) where
  unref = codecAnnotatedClassType

codecSet :: Ord a => Codec ValueCodec ctx a -> Codec ValueCodec ctx (Set.Set a)
codecSet c = bimap Set.toList Set.fromList $ manyOfList c

codecAnnotatedClassType :: HasV1 ctx => Codec ValueCodec ctx (Annotated ClassType)
codecAnnotatedClassType = broken

instance Def "Annotation" ValueCodec V1 Annotation where
  unref = objectAll (pure ())

codecAnnotated :: HasV1 ctx => Codec ObjectCodec ctx a -> Codec ValueCodec ctx (Annotated a)
codecAnnotated ca = objectAll do
  #_annotatedContent =: ca
  #_annotatedAnnotations ~ "annotations" <: manyOfList (ref @"Annotation")

instance Def "ThrowsType" ValueCodec V1 (Annotated ThrowsType) where
  unref = broken

instance Def "Type" ValueCodec V1 (Annotated Type) where
  unref = broken

instance Def "Class" ValueCodec V1 Class where unref = codecClass

codecClass :: HasV1 ctx => Codec ValueCodec ctx Class
codecClass = objectAll do
  #_className' ~ "name" <: ref @"ClassName"
  #_classAccessFlags ~ "access" <: broken
  #_classTypeParameters ~ "typeparams" <: manyOfList (ref @"TypeParameter")
  #_classSuper ~ "super" <: optional (ref @"ClassType")
  #_classInterfaces ~ "interfaces" <: manyOfList (ref @"ClassType")
  #_classFields ~ "fields" <: manyOfList (ref @"Field")
  #_classMethods ~ "methods" <: manyOfList (ref @"Method")
  #_classAnnotations ~ "annotations" <: manyOfList (ref @"Annotation")
  #_classVersion
    ~ "version"
    <: optional
      ( arrayAll do
          at @0 <: boundIntegral
          at @1 <: boundIntegral
      )

instance Def "Method" ValueCodec V1 Method where
  unref = objectAll do
    #_methodName ~ "name" <: text
    #_methodAccessFlags ~ "access" <: codecSet codecMAccessFlag
    #_methodTypeParameters ~ "typeparams" <: manyOfList (ref @"TypeParameter")
    #_methodParameters ~ "params" <: manyOfList (ref @"Parameter")
    #_methodReturnType ~ "returns" <: ref @"ReturnType"
    #_methodCode ~ "code" <: optional (ref @"Code")
    #_methodAnnotations ~ "annotations" <: manyOfList (ref @"Annotation")
    #_methodExceptions ~ "exceptions" <: manyOfList (ref @"ThrowsType")
    #_methodDefaultAnnotation
      ~ "default"
      <: optional (ref @"AnnotationValue")
   where
    codecMAccessFlag = any do
      #ifMPublic =: "public"
      #ifMPrivate =: "private"
      #ifMProtected =: "protected"
      #ifMStatic =: "static"
      #ifMFinal =: "final"
      #ifMSynchronized =: "synchronized"
      #ifMBridge =: "bridge"
      #ifMVarargs =: "varargs"
      #ifMNative =: "native"
      #ifMAbstract =: "abstract"
      #ifMStrictFP =: "strictFP"
      #ifMSynthetic =: "synthetic"

instance Def "Code" ValueCodec V1 Code where
  unref = objectAll do
    #_codeMaxStack ~ "max_stack" <: boundIntegral
    #_codeMaxLocals ~ "max_locals" <: boundIntegral
    #_codeExceptionTable ~ "exceptions" <: manyOfList codecExceptionHandler
    #_codeStackMap ~ "stack_map" <: broken
    #_codeByteCode ~ "bytecode" <: manyOf (ref @"ByteCodeInst")
   where
    codecExceptionHandler = objectAll do
      #_ehStart ~ "start" <: boundIntegral
      #_ehEnd ~ "end" <: boundIntegral
      #_ehHandler ~ "handler" <: boundIntegral
      #_ehCatchType ~ "catchType" <: optional (ref @"ClassName")

tbd :: Applicative m => m ()
tbd = pure ()

instance Def "JValue" ValueCodec V1 JValue where
  unref = object $ tagged "type" do
    #ifVInteger =: "integer" // ("value" ~: boundIntegral)
    #ifVLong =: "long" // ("value" ~: boundIntegral)
    #ifVFloat =: "float" // ("value" ~: boundIntegral)
    #ifVDouble =: "double" // ("value" ~: boundIntegral)
    #ifVString =: "string" // ("value" ~: text)
    #ifVClass =: "class" // ("value" ~: ref @"JRefType")
    #ifVMethodType =: "methodtype" // codecMethodDescriptor
    #ifVMethodHandle =: "methodhandle" // broken

instance Def "CmpOpr" ValueCodec V1 B.CmpOpr where
  unref = any do
    #ifCEq =: "eq"
    #ifCNe =: "ne"
    #ifCLt =: "lt"
    #ifCGe =: "ge"
    #ifCGt =: "gt"
    #ifCLe =: "le"

-- instance Def "AbsFieldId" ValueCodec V1 B.AbsFieldId where
--   unref = broken
--
-- instance Def "AbsMethodId" ValueCodec V1 B.AbsMethodId where
--   unref = broken

instance Def "ArrayType" ValueCodec V1 B.ArrayType where
  unref = any do
    #ifAByte =: "byte"
    #ifAChar =: "char"
    #ifAShort =: "short"
    #ifAInt =: "int"
    #ifALong =: "long"
    #ifAFloat =: "float"
    #ifADouble =: "double"
    #ifARef =: "ref"

codecInRefType
  :: Def "JRefType" ValueCodec ctx JRefType
  => Codec ObjectCodec ctx a
  -> Codec ObjectCodec ctx (B.InRefType a)
codecInRefType c = all do
  #inRefType ~ "ref" <: ref @"JRefType"
  #inRefTypeId =: c

codecInClass
  :: Def "ClassName" ValueCodec ctx ClassName
  => Codec ObjectCodec ctx a
  -> Codec ObjectCodec ctx (B.InClass a)
codecInClass c = all do
  #inClassName ~ "class" <: ref @"ClassName"
  #inClassId =: c

codecMethodId
  :: ( Def "JType" ValueCodec ctx JType
     )
  => Codec ObjectCodec ctx B.MethodId
codecMethodId = simply $ codecNameAndType codecMethodDescriptor

codecFieldId
  :: ( Def "JType" ValueCodec ctx JType
     )
  => Codec ObjectCodec ctx B.FieldId
codecFieldId = simply $ codecNameAndType ("type" ~: codecFieldDescriptor)

codecAbsFieldId
  :: ( Def "ClassName" ValueCodec ctx ClassName
     , Def "JType" ValueCodec ctx JType
     )
  => Codec ObjectCodec ctx B.AbsFieldId
codecAbsFieldId = simply $ codecInClass codecFieldId

codecMethodDescriptor
  :: Def "JType" ValueCodec ctx JType
  => Codec ObjectCodec ctx B.MethodDescriptor
codecMethodDescriptor = all do
  #methodDescriptorArguments ~ "args" <: manyOfList (ref @"JType")
  #methodDescriptorReturnType ~ "returns" <: simply (optional (ref @"JType"))

codecFieldDescriptor
  :: Def "JType" ValueCodec ctx JType
  => Codec ValueCodec ctx B.FieldDescriptor
codecFieldDescriptor = simply (ref @"JType")

codecAbsVariableMethodId
  :: ( Def "JType" ValueCodec ctx JType
     , Def "JRefType" ValueCodec ctx JRefType
     )
  => Codec ObjectCodec ctx B.AbsVariableMethodId
codecAbsVariableMethodId = all do
  #variableIsInterface ~ "is_interface" <: bool
  #variableMethodId =: codecInRefType codecMethodId

codecNameAndType
  :: Codec ObjectCodec ctx a
  -> Codec ObjectCodec ctx (B.NameAndType a)
codecNameAndType cdc = bimap (\(B.NameAndType t a) -> (t, a)) (uncurry B.NameAndType) $ all do
  #fst ~ "name" <: text
  #snd =: cdc

data BC = BC String [String] [String]

bc :: (Semigroup a, IsString a) => BC -> a
bc (BC name before after) = do
  "{" <> fromString name <> "} " <> fromString (show before) <> " -> " <> fromString (show after)

instance Def "ByteCodeInst" ValueCodec V1 ByteCodeInst where
  unref = objectAll do
    #offset <: boundIntegral
    #opcode =: tagged "opr" do
      given ifArrayStore
        =: ( "array_store" // do
              "type" ~: ref @"ArrayType"
           )
        <?> "load a $value of $type from an $arrayref array at index $index"
        <?> bc (BC "aastore" ["arrayref", "index"] ["value"])

      given ifArrayLoad
        =: ( "array_load" // do
              "type" ~: ref @"ArrayType"
           )
        <?> "store a $value of $type in a $arrayref array at index $index"
        <?> bc (BC "aaload" ["arrayref", "index", "value"] [])

      given ifPush
        =: ( "push" // do
              "value" ~: optional (ref @"JValue")
           )
        <?> "push a $value or null onto the stack"
        <?> bc (BC "*" [] ["value"])

      given ifLoad
        =: ( "load" // all do
              #fst ~ "type" <: ref @"LocalType"
              #snd ~ "index" <: boundIntegral
           )
        <?> "load a local variable $index of $type"
        <?> bc (BC "*" [] ["value"])

      given ifStore
        =: ( "store" // all do
              #fst ~ "type" <: ref @"LocalType"
              #snd ~ "index" <: boundIntegral
           )
        <?> "store a local variable $index of $type"
        <?> bc (BC "*" ["value"] [])

      given ifIncrLocal
        =: ( "incr" // all do
              #fst ~ "index" <: boundIntegral
              #snd ~ "amount" <: boundIntegral
           )
        <?> "increment local in $index by $anount"
        <?> bc (BC "iinc" [] [])

      given ifBinaryOpr
        =: ( "binary" // all do
              #snd ~ "type" <: ref @"ArithmeticType"
              #fst ~ "operant" <: any do
                given ifAdd =: "add"
                given ifSub =: "sub"
                given ifMul =: "mul"
                given ifDiv =: "div"
                given ifRem =: "rem"
           )
        <?> "does a binary operation over type $type, with $operant on $value1 $value2"
        <?> bc (BC "*" ["value1", "value2"] ["result"])

      given ifNeg
        =: ( "negate" // do
              "type" ~: ref @"ArithmeticType"
           )
        <?> "negate the $value of $type"
        <?> bc (BC "*" ["value"] ["result"])

      given ifBitOpr
        =: ( "bitopr" // all do
              #snd ~ "type" <: any do
                given ifOne =: "int"
                given ifTwo =: "long"
              #fst ~ "operant" <: any do
                given ifShL =: "shl"
                given ifShR =: "shr"
                given ifUShR =: "ushr"
                given ifAnd =: "and"
                given ifOr =: "or"
                given ifXOr =: "xor"
           )
        <?> "negate the $value of $type"
        <?> bc (BC "*" ["value"] ["result"])

      given ifCast
        =: ( "cast" // any do
              given ifCastDown
                =: untup
                  ( all do
                      #fst ~ "from" <: "int"
                      #snd ~ "to" <: any do
                        #ifMByte =: "byte"
                        #ifMChar =: "char"
                        #ifMShort =: "short"
                  )
              given ifCastTo =: all do
                #fst ~ "from" <: ref @"ArithmeticType"
                #snd ~ "to" <: ref @"ArithmeticType"
           )
        <?> "cast from one type to another, only int can be cast to a small type"

      given ifCompareLongs
        =: ("comparelongs" // emptyObject)
        <?> "compare two longs, return 1 if $value1 is larger than $value2 0 if equal and -1 if smaller in $result"
        <?> bc (BC "lcmp" ["value1", "value1"] ["result"])

      given ifCompareFloating
        =: ( "comparefloating" // all do
              #fst ~ "onnan" <: any do
                given ifTrue =: exact (Number 1)
                given ifFalse =: exact (Number (-1))
              #snd ~ "type" <: any do
                given ifOne =: "float"
                given ifTwo =: "double"
           )
        <?> "compare two floats, return 1 if $value1 is larger than $value2 0 if equal and -1 if smaller in $result"
        <?> "pushes $onnan$ if any is NaN"
        <?> bc (BC "*cmp" ["value1", "value1"] ["result"])

      given ifIf
        =: ( "if" // all do
              #fst ~ "condition" <: ref @"CmpOpr"
              #snd
                ~ "with_zero"
                <: ( bimap
                      (\case B.One -> True; B.Two -> False)
                      (\case False -> B.Two; True -> B.One)
                      bool
                      <?> "compare with zero or with an extra from the stack"
                   )
              #trd ~ "target" <: boundIntegral
           )
        <?> "jump to $target if the $condition over intergers is true, compare with two elements or against zero if $with_zero"
        <?> bc (BC "if_a*" [] [])

      given ifIfRef
        =: ( "ifref" // all do
              #fst ~ "condition" <: any do
                #ifTrue =: "eq"
                #ifFalse =: "neq"
              #snd
                ~ "with_null"
                <: ( bimap
                      (\case B.One -> True; B.Two -> False)
                      (\case False -> B.Two; True -> B.One)
                      bool
                      <?> "compare with zero or with an extra from the stack"
                   )
              #trd ~ "target" <: boundIntegral
           )
        <?> "jump to $target if the $condition is true, compare with two elements or against null if $with_null"
        <?> bc (BC "if_a*" ["objectref", "objectref"] ["objectref"])
        <?> bc (BC "ifnull" ["objectref"] ["objectref"])
        <?> bc (BC "ifnonnull" ["objectref"] ["objectref"])

      given ifGoto
        =: ( "goto"
              // ("target" ~: boundIntegral)
           )
        <?> "jump to $target"
        <?> bc (BC "goto*" [] [])

      given ifJsr
        =: ("jsr" // ("target" ~: boundIntegral))
        <?> "jump to $target an add an $address on stack, normaly not used"
        <?> bc (BC "jsr*" [] ["address"])

      given ifRet
        =: ( "ret"
              // ("local" ~: boundIntegral)
           )
        <?> "returns the execution to adress in $local - not return"
        <?> bc (BC "ret" [] [])

      given ifTableSwitch
        =: ( "tableswitch" // all do
              #fst ~ "default" <: boundIntegral
              #snd =: all do
                given getSwitchLow
                  ~ "low"
                  <: boundIntegral
                  <?> "value to substract from index before looking up"
                given getSwitchOffsets
                  ~ "targets"
                  <: manyOf boundIntegral
           )
        <?> "jump to $index - $low in $targets, otherwise jump to $default"
        <?> bc (BC "tableswitch" ["index"] [])

      given ifLookupSwitch
        =: ( "lookupswitch" // all do
              #fst ~ "default" <: boundIntegral
              #snd
                ~ "targets"
                <: manyOf
                  ( object $ all do
                      #fst ~ "key" <: boundIntegral
                      #fst ~ "target" <: boundIntegral
                  )
           )
        <?> "jump to $key = $index in $targets, otherwise jump to $default"
        <?> bc (BC "lookupswitch" ["index"] [])

      given ifGet
        =: ( "get" // all do
              #fst
                ~ "static"
                <: bimap
                  ( \case
                      B.FldStatic -> True
                      B.FldField -> False
                  )
                  ( \case
                      True -> B.FldStatic
                      False -> B.FldField
                  )
                  bool
              #snd ~ "field" <: object codecAbsFieldId
           )
        <?> "get value from $field (might be $static)"
        <?> bc (BC "getfield" ["objectref"] ["value"])
        <?> ("if static " <> bc (BC "getstatic" [] ["value"]))
      given ifPut
        =: ( "put" // all do
              #fst
                ~ "static"
                <: bimap
                  ( \case
                      B.FldStatic -> True
                      B.FldField -> False
                  )
                  ( \case
                      True -> B.FldStatic
                      False -> B.FldField
                  )
                  bool
              #snd ~ "field" <: object codecAbsFieldId
           )
        <?> "put a value into a $field (might be $static)"
        <?> bc (BC "putfield" ["objectref", "value"] [])
        <?> ("if static " <> bc (BC "putstatic" ["value"] []))
      given ifInvoke
        =: ( "invoke"
              // tagged "access" do
                given ifInvkSpecial
                  =: ("special" // codecAbsVariableMethodId)
                given ifInvkVirtual
                  =: ("virtual" // codecInRefType codecMethodId)
                given ifInvkStatic
                  =: ( "static" // codecAbsVariableMethodId
                     )
                given ifInvkInterface
                  =: ( "interface" // all do
                        #fst ~ "stack_size" <: boundIntegral
                        #snd =: simply (codecInRefType codecMethodId)
                     )
                given ifInvkDynamic
                  =: ( "dynamic" // all do
                        given getInvokeDynamicAttrIndex ~ "index" <: boundIntegral
                        given getInvokeDynamicMethod =: codecMethodId
                     )
           )
        <?> "invoke a method, consult the documentation"
        <?> bc (BC "invokevirtual" ["objectref", "arg1", "arg2", "..."] ["result"])
      given ifNew
        =: ( "new"
              // ("class" ~: ref @"ClassName")
           )
        <?> "create a new $object of $class"
        <?> bc (BC "new" [] ["objectref"])
      given ifNewArray
        =: ( "newarray"
              // bimap
                (\(B.NewArrayType a b) -> (a, b))
                (uncurry B.NewArrayType)
                ( all do
                    #fst ~ "dim" <: boundIntegral
                    #snd ~ "type" <: ref @"JType"
                )
              <?> "create a $dim - dimentional array of size $count and $type"
              <?> bc (BC "newarray" ["count1", "count2", "..."] ["objectref"])
           )
      given ifArrayLength
        =: ( "arraylength" // emptyObject
              <?> "finds the length of an array"
              <?> bc (BC "arraylength" ["arraylength"] ["length"])
           )
      given ifThrow
        =: ( "throw" // emptyObject
              <?> "throws an exception"
              <?> bc (BC "athrow" ["objectref"] ["objectref"])
           )
      given ifCheckCast
        =: ( "checkcast" // ("type" ~: ref @"JRefType")
              <?> "throws a ClassCastException if $objectref can be cast to type $type"
              <?> bc (BC "checkcast" ["objectref"] ["objectref"])
           )
      given ifInstanceOf
        =: ( "instanceof" // ("type" ~: ref @"JRefType")
              <?> "returns 1 if $objectref can be cast to type $type, otherwise 0"
              <?> bc (BC "checkcast" ["objectref"] ["result"])
           )
      given ifMonitor
        =: ( "monitor"
              // ("enter" ~: bool)
              <?> "enter or exit a monitor (according to $enter)"
              <?> bc (BC "monitorenter" ["monitor"] [])
              <?> bc (BC "monitorexit" ["monitor"] [])
           )
      given ifReturn
        =: ( "return"
              // ("type" ~: optional (ref @"LocalType"))
           )
        <?> "return a $value of $type"
        <?> bc (BC "*return" ["value"] [])
      given ifNop =: ("nop" // emptyObject <?> "No operation")
      given ifPop
        =: ( "pop"
              // ( "words"
                    ~: any do
                      given ifOne =: exact (Number 1)
                      given ifTwo =: exact (Number 2)
                 )
              <?> "pop $words words elements from the stack, "
              <?> bc (BC "pop1" ["value1"] [])
              <?> bc (BC "pop2" ["value1", "value2"] [])
              <?> bc (BC "pop2" ["value1:long"] [])
           )
      given ifDup
        =: ( "dup"
              // ( "words" ~: any do
                    given ifOne =: exact (Number 1)
                    given ifTwo =: exact (Number 2)
                 )
              <?> "duplicate $words from stack"
              <?> bc (BC "dup" ["value1"] ["value1", "value1"])
              <?> bc (BC "dup2" ["v1", "v2"] ["v1", "v2", "v1", "v2"])
              <?> bc (BC "dup2" ["value1:long"] ["value1:long", "value1:long"])
           )
      given ifDupX1
        =: ( "dup_x1"
              // ( "words" ~: any do
                    given ifOne =: exact (Number 1)
                    given ifTwo =: exact (Number 2)
                 )
              <?> "duplicate $words on the stack insert one word deep, "
              <?> bc (BC "dup_x1" ["v2", "v1"] ["v1", "v2", "v1"])
              <?> bc
                ( BC
                    "dup2_x1"
                    ["v3", "v2", "v1"]
                    ["v2", "v1", "v3", "v2", "v1"]
                )
              <?> bc
                ( BC
                    "dup2_x1"
                    ["v2", "value1:long"]
                    ["value1:long", "v2", "value1:long"]
                )
           )
      given ifDupX2
        =: ( "dup_x2"
              // ( "words" ~: any do
                    given ifOne =: exact (Number 1)
                    given ifTwo =: exact (Number 2)
                 )
              <?> "duplicate $words from the stack insert 2 words deep"
              <?> bc (BC "dup_x2" ["v3", "v2", "v1"] ["v1", "v3", "v2", "v1"])
           )
      given ifSwap
        =: ( "swap" // emptyObject
              <?> "swap two elements on the stack"
              <?> bc (BC "swap" ["v2", "v1"] ["v1", "v2"])
           )

instance Def "Field" ValueCodec V1 Field where
  unref = objectAll do
    #_fieldName ~ "name" <: text
    #_fieldAccessFlags ~ "access" <: codecSet codecFAccessFlag
    #_fieldType ~ "type" <: ref @"Type"
    #_fieldValue ~ "value" <: optional (ref @"Value")
    #_fieldAnnotations ~ "annotations" <: manyOfList (ref @"Annotation")
   where
    codecFAccessFlag = any do
      #ifFPublic =: "public"
      #ifFPrivate =: "private"
      #ifFProtected =: "protected"
      #ifFStatic =: "static"
      #ifFFinal =: "final"
      #ifFVolatile =: "volatile"
      #ifFTransient =: "transient"
      #ifFSynthetic =: "synthetic"
      #ifFEnum =: "enum"

instance Def "Value" ValueCodec V1 JValue where
  unref = broken

codecTextSerializeable :: B.TextSerializable x => Codec ValueCodec ctx x
codecTextSerializeable = dimap (pure . B.serialize) B.deserialize text
