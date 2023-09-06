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
module Jvmhs.Format.Codec (
  toJSONClass,
  toEncodingClass,
  parseJSONClass,
) where

-- class
import Jvmhs.Data.Class

-- conedec
import Conedec

import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.Types as Aeson
import Data.Bifunctor (first)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import Jvmhs.Data.Code
import Jvmhs.Data.Type
import qualified Language.JVM as B
import Prelude hiding (all, any, null)

data V1

instance Def "ClassName" ValueCodec V1 ClassName where unref = codecClassName

codecClassName :: Codec ValueCodec ctx ClassName
codecClassName =
  codecTextSerializeable
    <?> "a name of a class, packages seperated by '/'"
      <!> "java/util/Object"

instance Def "BaseType" ValueCodec V1 JBaseType where
  unref = any do
    #ifJTByte =: "byte"
    #ifJTChar =: "char"
    #ifJTDouble =: "double"
    #ifJTFloat =: "float"
    #ifJTInt =: "int"
    #ifJTLong =: "long"
    #ifJTShort =: "short"
    #ifJTBoolean =: "boolean"

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

instance Def "Parameter" ObjectCodec V1 Parameter where
  unref = all do
    #_parameterNameAndFlags =: any do
      #ifNothing =: emptyObject
      #ifJust =: all do
        #fst ~ "name" <: text
        #snd
          ~ "access"
          <: codecSet
            ( any do
                #ifPFinal =: "final"
                #ifPSynthetic =: "synthetic"
                #ifPMandated =: "mandated"
            )
    #_parameterVisible ~ "visible" <: bool
    #_parameterType ~ "type" <: codecAnnotated (ref @"Type")

codecSet :: Ord a => Codec ValueCodec ctx a -> Codec ValueCodec ctx (Set.Set a)
codecSet c = bimap Set.toList Set.fromList $ manyOfList c

instance Def "TypeVariable" ObjectCodec V1 TypeVariable where
  unref = all do
    #_typeVariableName ~ "name" <: simply text
    #_typeVariableBound ~ "bound" <: ref @"ClassName"

instance Def "Annotation" ValueCodec V1 Annotation where
  unref = objectAll do
    #_annotationType ~ "type" <: ref @"ClassName"
    #_annotationIsRuntimeVisible ~ "is_runtime_visible" <: bool
    #_annotationValues
      ~ "values"
      <: bimap
        (fmap (first AesonKey.fromText) . HashMap.toList)
        (HashMap.fromList . fmap (first AesonKey.toText))
        (mapOf (ref @"AnnotationValue"))

instance Def "AnnotationValue" ValueCodec V1 AnnotationValue where
  unref = object $ taggedInto "type" "value" do
    #ifAVByte =: "byte" // boundIntegral
    #ifAVChar =: "char" // boundIntegral
    #ifAVInt =: "int" // boundIntegral
    #ifAVLong =: "long" // boundIntegral
    #ifAVShort =: "short" // boundIntegral
    #ifAVDouble =: "double" // realFloat
    #ifAVFloat =: "float" // realFloat
    #ifAVBoolean
      =: "boolean"
      // ( boundIntegral
            <?> "0 means false and 1 means true"
         )
    #ifAVString =: "string" // byteStringUtf8
    #ifAVEnum =: "enum" // objectAll do
      given getEnumTypeName ~ "type" <: simply (ref @"SimpleType")
      given getEnumConstName ~ "name" <: text
    #ifAVClass
      =: "class"
      // simply (optional (ref @"SimpleType"))
      <?> "is normally only class"
    #ifAVAnnotation =: "annotation" // objectAll do
      #fst ~ "type" <: ref @"ClassName"
      #snd
        ~ "values"
        <: bimap
          (fmap (first AesonKey.fromText) . HashMap.toList)
          (HashMap.fromList . fmap (first AesonKey.toText))
          (mapOf (ref @"AnnotationValue"))
    #ifAVArray =: "array" // manyOfList (ref @"AnnotationValue")

codecAnnotated :: Def "Annotation" ValueCodec ctx Annotation => Codec ObjectCodec ctx a -> Codec ValueCodec ctx (Annotated a)
codecAnnotated ca = objectAll do
  #_annotatedContent =: ca
  #_annotatedAnnotations ~ "annotations" <: manyOfList (ref @"Annotation")

instance Def "ThrowsType" ObjectCodec V1 ThrowsType where
  unref = tagged "kind" do
    #ifThrowsClass =: "class" // ref @"ClassType"
    #ifThrowsTypeVariable =: "typevar" // ref @"TypeVariable"

instance Def "ReferenceType" ObjectCodec V1 ReferenceType where
  unref = tagged "kind" do
    #ifRefClassType =: "class" // ref @"ClassType"
    #ifRefTypeVariable =: "typevar" // ref @"TypeVariable"
    #ifRefArrayType =: "array" // "type" ~: simply (codecAnnotated $ ref @"Type")

instance Def "SimpleReferenceType" ValueCodec V1 JRefType where
  unref = object $ tagged "kind" do
    #ifJTClass =: "class" // "name" ~: ref @"ClassName"
    #ifJTArray =: "array" // "type" ~: ref @"SimpleType"

instance Def "TypeParameter" ObjectCodec V1 TypeParameter where
  unref = all do
    #_typeParameterName ~ "name" <: simply text
    #_typeParameterClassBound
      ~ "classbound"
      <: optional (codecAnnotated $ ref @"ThrowsType")
    #_typeParameterInterfaceBound
      ~ "interfacebound"
      <: manyOfList (codecAnnotated $ ref @"ThrowsType")

instance Def "ClassType" ObjectCodec V1 ClassType where
  unref = all do
    #_classTypeName ~ "name" <: text
    #_classTypeInner ~ "inner" <: optional (codecAnnotated $ ref @"ClassType")
    #_classTypeArguments ~ "args" <: manyOfList (codecAnnotated $ ref @"TypeArgument")

instance Def "TypeArgument" ObjectCodec V1 TypeArgument where
  unref = tagged "kind" do
    #ifAnyTypeArg =: "any" // emptyObject
    #ifExtendedTypeArg =: "extended" // "type" ~: codecAnnotated (ref @"ReferenceType")
    #ifImplementedTypeArg =: "implemented" // "type" ~: codecAnnotated (ref @"ReferenceType")
    #ifTypeArg =: "simple" // "type" ~: object (ref @"ReferenceType")

instance Def "Type" ObjectCodec V1 Type where
  unref = any do
    #ifBaseType =: "base" ~: ref @"BaseType"
    #ifReferenceType =: ref @"ReferenceType"

instance Def "SimpleType" ValueCodec V1 JType where
  unref = any do
    #ifJTBase =: ref @"BaseType"
    #ifJTRef =: ref @"SimpleReferenceType"

instance Def "Class" ValueCodec V1 Class where
  unref = objectAll do
    #_className' ~ "name" <: ref @"ClassName"
    #_classAccessFlags
      ~ "access"
      <: codecSet
        ( any do
            #ifCPublic =: "public"
            #ifCFinal =: "final"
            #ifCSuper =: "super"
            #ifCInterface =: "interface"
            #ifCAbstract =: "abstract"
            #ifCSynthetic =: "synthetic"
            #ifCAnnotation =: "annotation"
            #ifCEnum =: "enum"
            #ifCModule =: "module"
        )
    #_classTypeParameters ~ "typeparams" <: manyOfList (codecAnnotated $ ref @"TypeParameter")
    #_classSuper ~ "super" <: optional (codecAnnotated $ ref @"ClassType")
    #_classInterfaces ~ "interfaces" <: manyOfList (codecAnnotated $ ref @"ClassType")
    #_classFields ~ "fields" <: manyOfList (ref @"Field")
    #_classMethods ~ "methods" <: manyOfList (ref @"Method")
    #_classBootstrapMethods
      ~ "bootstrapmethods"
      <: bimap
        IntMap.toList
        IntMap.fromList
        ( manyOfList . object $ all do
            at @0 ~ "index" <: boundIntegral
            at @1 ~ "method" <: ref @"BootstrapMethod"
        )
    #_classEnclosingMethod
      ~ "enclosingmethod"
      <: optional
        ( object $ all do
            #fst ~ "class" <: ref @"ClassName"
            #snd ~ "method" <: optional (object $ ref @"MethodId")
        )
    #_classInnerClasses ~ "innerclasses" <: manyOfList (ref @"InnerClass")
    #_classAnnotations ~ "annotations" <: manyOfList (ref @"Annotation")
    #_classVersion
      ~ "version"
      <: optional
        ( arrayAll do
            at @0 <: boundIntegral
            at @1 <: boundIntegral
        )

instance Def "InnerClass" ValueCodec V1 InnerClass where
  unref = objectAll do
    #_innerClass ~ "class" <: ref @"ClassName"
    #_innerOuterClass ~ "outer" <: optional (ref @"ClassName")
    #_innerClassName ~ "name" <: optional text
    #_innerAccessFlags
      ~ "access"
      <: codecSet
        ( any do
            #ifICPublic =: "public"
            #ifICPrivate =: "private"
            #ifICProtected =: "protected"
            #ifICStatic =: "static"
            #ifICFinal =: "final"
            #ifICInterface =: "interface"
            #ifICAbstract =: "abstract"
            #ifICSynthetic =: "synthetic"
            #ifICAnnotation =: "annotation"
            #ifICEnum =: "enum"
        )

instance Def "Method" ValueCodec V1 Method where
  unref = objectAll do
    #_methodName ~ "name" <: text
    #_methodAccessFlags ~ "access" <: codecSet codecMAccessFlag
    #_methodTypeParameters ~ "typeparams" <: manyOfList (codecAnnotated $ ref @"TypeParameter")
    #_methodParameters ~ "params" <: manyOfList (codecAnnotated $ ref @"Parameter")
    #_methodReturnType ~ "returns" <: codecAnnotated ("type" ~: simply (optional . object $ ref @"Type"))
    #_methodCode ~ "code" <: optional (ref @"Code")
    #_methodAnnotations ~ "annotations" <: manyOfList (ref @"Annotation")
    #_methodExceptions ~ "exceptions" <: manyOfList (codecAnnotated $ ref @"ThrowsType")
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
    #_codeStackMap ~ "stack_map" <: optional (ref @"StackMapTable")
    #_codeByteCode ~ "bytecode" <: manyOf (ref @"ByteCodeInst")
   where
    codecExceptionHandler = objectAll do
      #_ehStart ~ "start" <: boundIntegral
      #_ehEnd ~ "end" <: boundIntegral
      #_ehHandler ~ "handler" <: boundIntegral
      #_ehCatchType ~ "catchType" <: optional (ref @"ClassName")

instance Def "VerificationTypeInfo" ValueCodec V1 (VerificationTypeInfo B.High) where
  unref = object $ tagged "type" do
    given ifVTTop =: "top" // emptyObject
    given ifVTInteger =: "integer" // emptyObject
    given ifVTFloat =: "float" // emptyObject
    given ifVTLong =: "long" // emptyObject
    given ifVTDouble =: "double" // emptyObject
    given ifVTNull =: "null" // emptyObject
    given ifVTUninitializedThis =: "uninitializedthis" // emptyObject
    given ifVTUninitialized
      =: "uninitialized"
      // "index"
      ~: ( boundIntegral
            <?> "referes to the bytecode instruction that initialized it"
         )
    given ifVTObject =: "object" // "ref" ~: ref @"SimpleReferenceType"

instance Def "StackMapTable" ValueCodec V1 (StackMapTable B.High) where
  unref =
    simply
      ( manyOfList
          ( objectAll do
              given getDeltaOffset ~ "index" <: boundIntegral
              given getFrameType =: tagged "type" do
                #ifSameFrame =: "same" // emptyObject
                #ifSameLocals1StackItemFrame =: "same_locals_1_stack_item_frame" // "info" ~: ref @"VerificationTypeInfo"
                #ifChopFrame =: "chop_frame" // "info" ~: boundIntegral
                #ifAppendFrame =: "append_frame" // "info" ~: manyOfList (ref @"VerificationTypeInfo")
                #ifFullFrame =: "full_frame" // all do
                  #fst ~ "locals" <: simply (manyOfList $ ref @"VerificationTypeInfo")
                  #snd ~ "stack" <: simply (manyOfList $ ref @"VerificationTypeInfo")
          )
      )
      <?> "see https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.7.4 for more info"

instance Def "CmpOpr" ValueCodec V1 B.CmpOpr where
  unref = any do
    #ifCEq =: "eq"
    #ifCNe =: "ne"
    #ifCLt =: "lt"
    #ifCGe =: "ge"
    #ifCGt =: "gt"
    #ifCLe =: "le"

instance Def "MethodHandle" ValueCodec V1 (B.MethodHandle B.High) where
  unref = object $ tagged "handletype" do
    given ifMHField =: "field" // all do
      given getMethodHandleFieldKind ~ "kind" <: any do
        given ifMHGetField =: "getfield"
        given ifMHGetStatic =: "getstatic"
        given ifMHPutField =: "putfield"
        given ifMHPutStatic =: "putstatic"
      given getMethodHandleFieldRef ~ "ref" <: ref @"AbsFieldId"
    given ifMHMethod =: "method" // taggedInto "kind" "method" do
      given ifMHInvokeVirtual =: "virtual" // ref @"RefMethodId"
      given ifMHInvokeStatic =: "static" // ref @"RefVariableMethodId"
      given ifMHInvokeSpecial =: "special" // ref @"RefVariableMethodId"
      given ifMHNewInvokeSpecial =: "new_special" // ref @"RefMethodId"
    given ifMHInterface =: "interface" // "method" ~: simply (ref @"RefMethodId")

instance Def "JArrayType" ValueCodec V1 B.ArrayType where
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
  :: Def "SimpleReferenceType" ValueCodec ctx JRefType
  => Codec ObjectCodec ctx a
  -> Codec ObjectCodec ctx (B.InRefType a)
codecInRefType c = all do
  #inRefType ~ "ref" <: ref @"SimpleReferenceType"
  #inRefTypeId =: c

codecInClass
  :: Def "ClassName" ValueCodec ctx ClassName
  => Codec ObjectCodec ctx a
  -> Codec ObjectCodec ctx (B.InClass a)
codecInClass c = all do
  #inClassName ~ "class" <: ref @"ClassName"
  #inClassId =: c

codecFieldId
  :: ( Def "SimpleType" ValueCodec ctx JType
     )
  => Codec ObjectCodec ctx B.FieldId
codecFieldId = simply $ codecNameAndType ("type" ~: codecFieldDescriptor)

instance Def "AbsFieldId" ValueCodec V1 B.AbsFieldId where
  unref = object codecAbsFieldId

codecAbsFieldId
  :: ( Def "ClassName" ValueCodec ctx ClassName
     , Def "SimpleType" ValueCodec ctx JType
     )
  => Codec ObjectCodec ctx B.AbsFieldId
codecAbsFieldId = simply $ codecInClass codecFieldId

codecMethodDescriptor
  :: Def "SimpleType" ValueCodec ctx JType
  => Codec ObjectCodec ctx B.MethodDescriptor
codecMethodDescriptor = all do
  #methodDescriptorArguments ~ "args" <: manyOfList (ref @"SimpleType")
  #methodDescriptorReturnType ~ "returns" <: simply (optional (ref @"SimpleType"))

codecFieldDescriptor
  :: Def "SimpleType" ValueCodec ctx JType
  => Codec ValueCodec ctx B.FieldDescriptor
codecFieldDescriptor = simply (ref @"SimpleType")

instance Def "RefMethodId" ValueCodec V1 (B.InRefType B.MethodId) where
  unref = object codecRefMethodId

instance Def "ClassMethodId" ValueCodec V1 (B.InClass B.MethodId) where
  unref = object codecClassMethodId

codecRefMethodId
  :: ( Def "MethodId" ObjectCodec ctx B.MethodId
     , Def "SimpleReferenceType" ValueCodec ctx JRefType
     )
  => Codec ObjectCodec ctx (B.InRefType B.MethodId)
codecRefMethodId = codecInRefType (ref @"MethodId")

codecClassMethodId
  :: ( Def "MethodId" ObjectCodec ctx B.MethodId
     , Def "ClassName" ValueCodec ctx ClassName
     )
  => Codec ObjectCodec ctx (B.InClass B.MethodId)
codecClassMethodId = codecInClass (ref @"MethodId")

instance Def "MethodId" ObjectCodec V1 B.MethodId where
  unref = codecMethodId

codecMethodId
  :: ( Def "SimpleType" ValueCodec ctx JType
     )
  => Codec ObjectCodec ctx B.MethodId
codecMethodId = simply $ codecNameAndType codecMethodDescriptor

instance Def "RefVariableMethodId" ValueCodec V1 B.AbsVariableMethodId where
  unref = object codecAbsVariableMethodId

codecAbsVariableMethodId
  :: ( Def "MethodId" ObjectCodec ctx B.MethodId
     , Def "SimpleReferenceType" ValueCodec ctx JRefType
     )
  => Codec ObjectCodec ctx B.AbsVariableMethodId
codecAbsVariableMethodId = all do
  #variableIsInterface ~ "is_interface" <: bool
  #variableMethodId =: codecRefMethodId

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
              "type" ~: ref @"JArrayType"
           )
        <?> "load a $value of $type from an $arrayref array at index $index"
        <?> bc (BC "aastore" ["arrayref", "index"] ["value"])

      given ifArrayLoad
        =: ( "array_load" // do
              "type" ~: ref @"JArrayType"
           )
        <?> "store a $value of $type in a $arrayref array at index $index"
        <?> bc (BC "aaload" ["arrayref", "index", "value"] [])

      given ifPush
        =: ( "push" // do
              "value" ~: optional (ref @"Value")
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
                      #snd ~ "target" <: boundIntegral
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
                  =: ("special" // "method" ~: ref @"RefVariableMethodId")
                given ifInvkVirtual
                  =: ("virtual" // "method" ~: ref @"RefMethodId")
                given ifInvkStatic
                  =: ("static" // "method" ~: ref @"RefVariableMethodId")
                given ifInvkInterface
                  =: ( "interface" // all do
                        #fst ~ "stack_size" <: boundIntegral
                        #snd ~ "method" <: simply (ref @"RefMethodId")
                     )
                given ifInvkDynamic
                  =: ( "dynamic" // all do
                        given getInvokeDynamicAttrIndex ~ "index" <: boundIntegral
                        given getInvokeDynamicMethod ~ "method" <: object (ref @"MethodId")
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
                    #snd ~ "type" <: ref @"SimpleType"
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
        =: ( "checkcast" // ("type" ~: ref @"SimpleReferenceType")
              <?> "throws a ClassCastException if $objectref can be cast to type $type"
              <?> bc (BC "checkcast" ["objectref"] ["objectref"])
           )
      given ifInstanceOf
        =: ( "instanceof" // ("type" ~: ref @"SimpleReferenceType")
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
    #_fieldType ~ "type" <: codecAnnotated (ref @"Type")
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
  unref = object $ tagged "type" do
    #ifVInteger =: "integer" // ("value" ~: boundIntegral)
    #ifVLong =: "long" // ("value" ~: boundIntegral)
    #ifVFloat =: "float" // ("value" ~: realFloat)
    #ifVDouble =: "double" // ("value" ~: realFloat)
    #ifVString =: "string" // ("value" ~: byteStringUtf8)
    #ifVClass =: "class" // ("value" ~: ref @"SimpleReferenceType")
    #ifVMethodType =: "methodtype" // ("value" ~: object codecMethodDescriptor)
    #ifVMethodHandle =: "methodhandle" // ("value" ~: ref @"MethodHandle")

instance Def "BootstrapMethod" ValueCodec V1 BootstrapMethod where
  unref = object $ all do
    #_bootstrapMethodHandle ~ "handle" <: ref @"MethodHandle"
    #_bootstrapMethodArguments ~ "args" <: manyOfList (ref @"Value")

codecTextSerializeable :: B.TextSerializable x => Codec ValueCodec ctx x
codecTextSerializeable = dimap (pure . B.serialize) B.deserialize text

toJSONClass :: Class -> Either String Value
toJSONClass = runWithError . toJSONViaCodec @V1 (ref @"Class")

toEncodingClass :: Class -> Either String Aeson.Encoding
toEncodingClass = runWithError . toEncodingViaCodec @V1 (ref @"Class")

parseJSONClass :: Value -> Aeson.Parser Class
parseJSONClass = parseJSONViaCodec @V1 (ref @"Class")
