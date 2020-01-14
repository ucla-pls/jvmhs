{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE QuantifiedConstraints         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TypeApplications            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-|
Module      : Jvmhs.Inspection.ClassNames
Copyright   : (c) Christian Gram Kalhauge, 2019
License     : BSD-3-Clause
Maintainer  : kalhauge@cs.ucla.edu

-}
module Jvmhs.Inspection.ClassNames where

-- base
import           Data.Functor
import           Data.Foldable

-- lens 
import           Control.Lens
import           Data.Set.Lens

-- text
import qualified Data.Text                     as Text

-- containers 
import qualified Data.Set                      as Set

-- jvm-binary
import qualified Language.JVM                  as B

-- jvmhs
import           Jvmhs.Data.Class
import           Jvmhs.Data.Type
import           Jvmhs.Data.Code
import           Jvmhs.Data.Identifier

type ClassNamer a = forall r . Monoid r => Getting r a ClassName

classNames :: ClassNamer a -> a -> Set.Set ClassName
classNames = setOf

-- | A fold over all classnames mentioned in a class.
classNamesOfClass :: ClassNamer Class
classNamesOfClass = fold
  [ className
  , classTypeParameters
  . folded
  . classNamesOfAnnotated classNamesOfTypeParameter
  , classSuper . _Just . classNamesOfAnnotated classNamesOfClassType
  , classInterfaces . folded . classNamesOfAnnotated classNamesOfClassType
  , classFields . folded . classNamesOfField
  , classMethods . folded . classNamesOfMethod
  , classBootstrapMethods . folded . classNamesOfBootstrapMethod
  , classEnclosingMethod . _Just . (_1 <> _2 . _Just . classNamesOfMethodId)
  , classInnerClasses . folded . classNamesOfInnerClass
  , classAnnotations . folded . classNamesOfAnnotation
  ]

classNamesOfField :: ClassNamer Field
classNamesOfField = fold
  [ fieldType . classNamesOfAnnotated classNamesOfType
  , fieldValue . _Just . classNamesOfJValue
  , fieldAnnotations . folded . classNamesOfAnnotation
  ]

classNamesOfMethod :: ClassNamer Method
classNamesOfMethod = fold
  [ methodTypeParameters
  . folded
  . classNamesOfAnnotated classNamesOfTypeParameter
  , methodParameters . folded . classNamesOfParameter
  , methodReturnType
    . classNamesOfAnnotated (coerced . _Just . classNamesOfType)
  , methodCode . _Just . classNamesOfCode
  , methodExceptions . folded . classNamesOfAnnotated classNamesOfThrowsType
  , methodAnnotations . folded . classNamesOfAnnotation
  ]

classNamesOfParameter :: ClassNamer Parameter
classNamesOfParameter = fold
  [ parameterType . classNamesOfAnnotated classNamesOfType
  , parameterAnnotations . folded . classNamesOfAnnotation
  ]

classNamesOfInnerClass :: ClassNamer InnerClass
classNamesOfInnerClass = fold [innerClass, innerOuterClass . _Just]

classNamesOfBootstrapMethod :: ClassNamer BootstrapMethod
classNamesOfBootstrapMethod = fold
  [ bootstrapMethodHandle . classNamesOfMethodHandle
  , bootstrapMethodArguments . folded . classNamesOfJValue
  ]

classNamesOfMethodHandle :: ClassNamer (B.MethodHandle B.High)
classNamesOfMethodHandle f x = case x of
  MHField  (B.MethodHandleField _ r) -> classNamesOfAbsFieldId f r $> x
  MHMethod a                         -> case a of
    B.MHInvokeVirtual r ->
      (classNamesOfInRefType classNamesOfMethodId) f r $> x
    B.MHInvokeStatic  r -> classNamesOfAbsVariableMethodId f r $> x
    B.MHInvokeSpecial r -> classNamesOfAbsVariableMethodId f r $> x
    B.MHNewInvokeSpecial r ->
      (classNamesOfInRefType classNamesOfMethodId) f r $> x
  MHInterface a ->
    (coerced @(B.MethodHandleInterface B.High) @(B.MethodHandleInterface B.High)
      . classNamesOfInRefType classNamesOfMethodId
      )
        f
        a
      $> x

classNamesOfInClass :: ClassNamer a -> ClassNamer (InClass a)
classNamesOfInClass fld = inClassNameL <> inClassIdL . fld

classNamesOfInRefType :: ClassNamer a -> ClassNamer (InRefType a)
classNamesOfInRefType fld =
  inRefTypeL . classNamesOfJRefType <> inRefTypeIdL . fld

classNamesOfAbsVariableMethodId :: ClassNamer B.AbsVariableMethodId
classNamesOfAbsVariableMethodId =
  to B.variableMethodId . classNamesOfInRefType classNamesOfMethodId

classNamesOfAbsFieldId :: ClassNamer AbsFieldId
classNamesOfAbsFieldId =
  coerced @AbsFieldId @AbsFieldId . classNamesOfInClass classNamesOfFieldId

classNamesOfCode :: ClassNamer Code
classNamesOfCode = fold
  [ codeByteCode . folded . to B.opcode . classNamesOfByteCodeOpr
  , codeExceptionTable . folded . ehCatchType . _Just
  , codeStackMap . _Just . classNamesOfStackMapTable
  ]
 where
  classNamesOfByteCodeOpr f a = case a of
    B.Push c  -> classNamesOfBConstant f c $> a
    B.Get _ r -> classNamesOfAbsFieldId f r $> a
    B.Put _ r -> classNamesOfAbsFieldId f r $> a
    B.Invoke r -> classNamesOfInvocation f r $> a
    B.New r   -> f r $> a
    B.NewArray (B.NewArrayType _ r) -> classNamesOfJType f r $> a
    B.CheckCast r -> classNamesOfJRefType f r $> a
    B.InstanceOf r -> classNamesOfJRefType f r $> a
    _         -> pure a


  classNamesOfBConstant = _Just . classNamesOfJValue

  classNamesOfInvocation f a = case a of
    B.InvkSpecial r     -> classNamesOfAbsVariableMethodId f r $> a
    B.InvkVirtual r     -> (classNamesOfInRefType classNamesOfMethodId) f r $> a
    B.InvkStatic  r     -> classNamesOfAbsVariableMethodId f r $> a
    B.InvkInterface _ r -> classNamesOfAbsInterfaceMethodId f r $> a
    B.InvkDynamic r ->
      (to B.invokeDynamicMethod . classNamesOfMethodId) f r $> a

  classNamesOfStackMapTable =
    verificationTypeInfo
      . (\f c -> case c of
          VTObject n -> classNamesOfJRefType f n $> c
          _          -> pure c
        )

classNamesOfAbsInterfaceMethodId :: ClassNamer B.AbsInterfaceMethodId
classNamesOfAbsInterfaceMethodId =
  coerced @B.AbsInterfaceMethodId @B.AbsInterfaceMethodId
    . classNamesOfInRefType classNamesOfMethodId


classNamesOfMethodId :: ClassNamer MethodId
classNamesOfMethodId = methodIdDescriptor . classNamesOfMethodDescriptor

classNamesOfFieldId :: ClassNamer FieldId
classNamesOfFieldId = fieldIdDescriptor . classNamesOfFieldDescriptor

classNamesOfJValue :: ClassNamer JValue
classNamesOfJValue f a = case a of
  VClass        r -> classNamesOfJRefType f r $> a
  VMethodType   r -> classNamesOfMethodDescriptor f r $> a
  VMethodHandle r -> classNamesOfMethodHandle f r $> a
  _               -> pure a


classNamesOfAnnotated :: ClassNamer a -> ClassNamer (Annotated a)
classNamesOfAnnotated fld = fold
  [ annotatedAnnotations . folded . classNamesOfAnnotation
  , annotatedContent . fld
  ]

classNamesOfTypeParameter :: ClassNamer TypeParameter
classNamesOfTypeParameter = fold
  [ typeParameterClassBound
  . _Just
  . classNamesOfAnnotated classNamesOfThrowsType
  , typeParameterInterfaceBound
  . folded
  . classNamesOfAnnotated classNamesOfThrowsType
  ]

classNamesOfAnnotation :: ClassNamer Annotation
classNamesOfAnnotation =
  fold [annotationType, annotationValues . folded . classNamesOfAnnotationValue]

classNamesOfAnnotationValue :: ClassNamer AnnotationValue
classNamesOfAnnotationValue = fold
  [ _AAnnotation . (_1 <> _2 . folded . classNamesOfAnnotationValue)
  , _AArray . folded . classNamesOfAnnotationValue
  , _AClass . coerced . _Just . classNamesOfJType
  , _AEnum . to enumTypeName . coerced . classNamesOfJType
  ]

classNamesOfFieldDescriptor :: ClassNamer FieldDescriptor
classNamesOfFieldDescriptor = coerced . classNamesOfJType

classNamesOfMethodDescriptor :: ClassNamer MethodDescriptor
classNamesOfMethodDescriptor =
  methodDArguments
    .  folded
    .  classNamesOfJType
    <> methodDReturnType
    .  _Just
    .  classNamesOfJType

classNamesOfThrowsType :: ClassNamer ThrowsType
classNamesOfThrowsType = fold
  [ _ThrowsClass . classNamesOfClassType
  , _ThrowsTypeVariable . classNamesOfTypeVariable
  ]

classNamesOfClassType :: ClassNamer ClassType
classNamesOfClassType = go ""
 where
  go :: Text.Text -> ClassNamer ClassType
  go n f ct =
    let n' = n <> ct ^. classTypeName
    in
      fold
        [ _Right f (textCls n') $> ct
        , ( classTypeArguments
          . folded
          . classNamesOfAnnotated classNamesOfTypeArgument
          )
          f
          ct
        , (classTypeInner . _Just . classNamesOfAnnotated (go n')) f ct
        ]

classNamesOfType :: ClassNamer Type
classNamesOfType = _ReferenceType . classNamesOfReferenceType

classNamesOfJType :: ClassNamer JType
classNamesOfJType = _JTRef . classNamesOfJRefType

classNamesOfJRefType :: ClassNamer JRefType
classNamesOfJRefType = fold [_JTClass, _JTArray . classNamesOfJType]

classNamesOfReferenceType :: ClassNamer ReferenceType
classNamesOfReferenceType = fold
  [ _RefClassType . classNamesOfClassType
  , _RefTypeVariable . classNamesOfTypeVariable
  , _RefArrayType . classNamesOfArrayType
  ]

classNamesOfTypeVariable :: ClassNamer TypeVariable
classNamesOfTypeVariable = typeVariableBound

classNamesOfArrayType :: ClassNamer ArrayType
classNamesOfArrayType = arrayType . classNamesOfAnnotated classNamesOfType

classNamesOfTypeArgument :: ClassNamer TypeArgument
classNamesOfTypeArgument = fold
  [ _ExtendedTypeArg . classNamesOfAnnotated classNamesOfReferenceType
  , _ImplementedTypeArg . classNamesOfAnnotated classNamesOfReferenceType
  , _TypeArg . classNamesOfReferenceType
  ]
