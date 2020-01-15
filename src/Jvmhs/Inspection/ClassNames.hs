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

classNameSet :: HasClassNames a => a -> Set.Set ClassName
classNameSet = setOf classNames

class HasClassNames a where
  classNames :: forall r . Monoid r => Getting r a ClassName

instance HasClassNames ClassName where
  classNames = id
  {-# INLINE classNames #-}

instance HasClassNames Class where
  classNames = classNamesOfClass
  {-# INLINE classNames #-}

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

instance HasClassNames Field where
  classNames = classNamesOfField
  {-# INLINE classNames #-}

classNamesOfField :: ClassNamer Field
classNamesOfField = fold
  [ fieldType . classNamesOfAnnotated classNamesOfType
  , fieldValue . _Just . classNamesOfJValue
  , fieldAnnotations . folded . classNamesOfAnnotation
  ]

instance HasClassNames Method where
  classNames = classNamesOfMethod
  {-# INLINE classNames #-}

classNamesOfMethod :: ClassNamer Method
classNamesOfMethod = fold
  [ methodTypeParameters
  . folded
  . classNamesOfAnnotated classNamesOfTypeParameter
  , methodParameters . folded . classNamesOfParameter
  , methodReturnType . classNamesOfAnnotated classNamesOfReturnType
  , methodCode . _Just . classNamesOfCode
  , methodExceptions . folded . classNamesOfAnnotated classNamesOfThrowsType
  , methodAnnotations . folded . classNamesOfAnnotation
  ]

instance HasClassNames ReturnType where
  classNames = classNamesOfReturnType
  {-# INLINE classNames #-}

classNamesOfReturnType :: ClassNamer ReturnType
classNamesOfReturnType = coerced . _Just . classNamesOfType

instance HasClassNames Parameter where
  classNames = classNamesOfParameter
  {-# INLINE classNames #-}

classNamesOfParameter :: ClassNamer Parameter
classNamesOfParameter = fold
  [ parameterType . classNamesOfAnnotated classNamesOfType
  , parameterAnnotations . folded . classNamesOfAnnotation
  ]

instance HasClassNames InnerClass where
  classNames = classNamesOfInnerClass
  {-# INLINE classNames #-}

classNamesOfInnerClass :: ClassNamer InnerClass
classNamesOfInnerClass = fold [innerClass, innerOuterClass . _Just]

instance HasClassNames BootstrapMethod where
  classNames = classNamesOfBootstrapMethod
  {-# INLINE classNames #-}

classNamesOfBootstrapMethod :: ClassNamer BootstrapMethod
classNamesOfBootstrapMethod = fold
  [ bootstrapMethodHandle . classNamesOfMethodHandle
  , bootstrapMethodArguments . folded . classNamesOfJValue
  ]

instance HasClassNames (B.MethodHandle B.High) where
  classNames = classNamesOfMethodHandle
  {-# INLINE classNames #-}

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

instance HasClassNames a => HasClassNames (InClass a) where
  classNames = classNamesOfInClass classNames
  {-# INLINE classNames #-}

classNamesOfInClass :: ClassNamer a -> ClassNamer (InClass a)
classNamesOfInClass fld = inClassNameL <> inClassIdL . fld

instance HasClassNames a => HasClassNames (InRefType a) where
  classNames = classNamesOfInRefType classNames
  {-# INLINE classNames #-}

classNamesOfInRefType :: ClassNamer a -> ClassNamer (InRefType a)
classNamesOfInRefType fld =
  inRefTypeL . classNamesOfJRefType <> inRefTypeIdL . fld

instance HasClassNames B.AbsVariableMethodId where
  classNames = classNamesOfAbsVariableMethodId
  {-# INLINE classNames #-}

classNamesOfAbsVariableMethodId :: ClassNamer B.AbsVariableMethodId
classNamesOfAbsVariableMethodId =
  to B.variableMethodId . classNamesOfInRefType classNamesOfMethodId

instance HasClassNames B.AbsInterfaceMethodId where
  classNames = classNamesOfAbsInterfaceMethodId
  {-# INLINE classNames #-}

classNamesOfAbsInterfaceMethodId :: ClassNamer B.AbsInterfaceMethodId
classNamesOfAbsInterfaceMethodId =
  coerced @B.AbsInterfaceMethodId @B.AbsInterfaceMethodId
    . classNamesOfInRefType classNamesOfMethodId

instance HasClassNames AbsFieldId where
  classNames = classNamesOfAbsFieldId
  {-# INLINE classNames #-}

classNamesOfAbsFieldId :: ClassNamer AbsFieldId
classNamesOfAbsFieldId =
  coerced @AbsFieldId @AbsFieldId . classNamesOfInClass classNamesOfFieldId

instance HasClassNames AbsMethodId where
  classNames = classNamesOfAbsMethodId
  {-# INLINE classNames #-}

classNamesOfAbsMethodId :: ClassNamer AbsMethodId
classNamesOfAbsMethodId =
  coerced @AbsMethodId @AbsMethodId . classNamesOfInClass classNamesOfMethodId

instance HasClassNames Code where
  classNames = classNamesOfCode
  {-# INLINE classNames #-}

classNamesOfCode :: ClassNamer Code
classNamesOfCode = fold
  [ codeByteCode . folded . classNamesOfByteCodeInst
  , codeExceptionTable . folded . classNamesOfExceptionHandler
  , codeStackMap . _Just . classNamesOfStackMapTable
  ]

instance HasClassNames ExceptionHandler where
  classNames = classNamesOfExceptionHandler
  {-# INLINE classNames #-}

classNamesOfExceptionHandler :: ClassNamer ExceptionHandler
classNamesOfExceptionHandler = ehCatchType . _Just

instance HasClassNames (B.ByteCodeInst B.High) where
  classNames = classNamesOfByteCodeInst
  {-# INLINE classNames #-}

classNamesOfByteCodeInst :: ClassNamer (B.ByteCodeInst B.High)
classNamesOfByteCodeInst = to B.opcode . classNamesOfByteCodeOpr

instance HasClassNames (B.ByteCodeOpr B.High) where
  classNames = classNamesOfByteCodeOpr
  {-# INLINE classNames #-}

classNamesOfByteCodeOpr :: ClassNamer (B.ByteCodeOpr B.High)
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
 where
  classNamesOfBConstant = _Just . classNamesOfJValue

  classNamesOfInvocation f' a' = case a' of
    B.InvkSpecial r     -> classNamesOfAbsVariableMethodId f' r $> a
    B.InvkVirtual r -> (classNamesOfInRefType classNamesOfMethodId) f' r $> a
    B.InvkStatic  r     -> classNamesOfAbsVariableMethodId f' r $> a
    B.InvkInterface _ r -> classNamesOfAbsInterfaceMethodId f' r $> a
    B.InvkDynamic r ->
      (to B.invokeDynamicMethod . classNamesOfMethodId) f' r $> a

instance HasClassNames (B.StackMapTable B.High) where
  classNames = classNamesOfStackMapTable
  {-# INLINE classNames #-}

classNamesOfStackMapTable :: ClassNamer (B.StackMapTable B.High)
classNamesOfStackMapTable =
  verificationTypeInfo
    . (\f c -> case c of
        VTObject n -> classNamesOfJRefType f n $> c
        _          -> pure c
      )


instance HasClassNames MethodId where
  classNames = classNamesOfMethodId
  {-# INLINE classNames #-}

classNamesOfMethodId :: ClassNamer MethodId
classNamesOfMethodId = methodIdDescriptor . classNamesOfMethodDescriptor

instance HasClassNames FieldId where
  classNames = classNamesOfFieldId
  {-# INLINE classNames #-}

classNamesOfFieldId :: ClassNamer FieldId
classNamesOfFieldId = fieldIdDescriptor . classNamesOfFieldDescriptor

instance HasClassNames JValue where
  classNames = classNamesOfJValue
  {-# INLINE classNames #-}

classNamesOfJValue :: ClassNamer JValue
classNamesOfJValue f a = case a of
  VClass        r -> classNamesOfJRefType f r $> a
  VMethodType   r -> classNamesOfMethodDescriptor f r $> a
  VMethodHandle r -> classNamesOfMethodHandle f r $> a
  _               -> pure a

instance HasClassNames a => HasClassNames (Annotated a) where
  classNames = classNamesOfAnnotated classNames
  {-# INLINE classNames #-}

classNamesOfAnnotated :: ClassNamer a -> ClassNamer (Annotated a)
classNamesOfAnnotated fld = fold
  [ annotatedAnnotations . folded . classNamesOfAnnotation
  , annotatedContent . fld
  ]

instance HasClassNames TypeParameter where
  classNames = classNamesOfTypeParameter
  {-# INLINE classNames #-}

classNamesOfTypeParameter :: ClassNamer TypeParameter
classNamesOfTypeParameter = fold
  [ typeParameterClassBound
  . _Just
  . classNamesOfAnnotated classNamesOfThrowsType
  , typeParameterInterfaceBound
  . folded
  . classNamesOfAnnotated classNamesOfThrowsType
  ]

instance HasClassNames Annotation where
  classNames = classNamesOfAnnotation
  {-# INLINE classNames #-}

classNamesOfAnnotation :: ClassNamer Annotation
classNamesOfAnnotation =
  fold [annotationType, annotationValues . folded . classNamesOfAnnotationValue]

instance HasClassNames AnnotationValue where
  classNames = classNamesOfAnnotationValue
  {-# INLINE classNames #-}

classNamesOfAnnotationValue :: ClassNamer AnnotationValue
classNamesOfAnnotationValue = fold
  [ _AAnnotation . (_1 <> _2 . folded . classNamesOfAnnotationValue)
  , _AArray . folded . classNamesOfAnnotationValue
  , _AClass . coerced . _Just . classNamesOfJType
  , _AEnum . to enumTypeName . coerced . classNamesOfJType
  ]

instance HasClassNames FieldDescriptor where
  classNames = classNamesOfFieldDescriptor
  {-# INLINE classNames #-}

classNamesOfFieldDescriptor :: ClassNamer FieldDescriptor
classNamesOfFieldDescriptor = coerced . classNamesOfJType

instance HasClassNames MethodDescriptor where
  classNames = classNamesOfMethodDescriptor
  {-# INLINE classNames #-}

classNamesOfMethodDescriptor :: ClassNamer MethodDescriptor
classNamesOfMethodDescriptor =
  methodDArguments
    .  folded
    .  classNamesOfJType
    <> methodDReturnType
    .  _Just
    .  classNamesOfJType

instance HasClassNames ThrowsType where
  classNames = classNamesOfThrowsType
  {-# INLINE classNames #-}

classNamesOfThrowsType :: ClassNamer ThrowsType
classNamesOfThrowsType = fold
  [ _ThrowsClass . classNamesOfClassType
  , _ThrowsTypeVariable . classNamesOfTypeVariable
  ]

instance HasClassNames ClassType where
  classNames = classNamesOfClassType
  {-# INLINE classNames #-}

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

instance HasClassNames Type where
  classNames = classNamesOfType
  {-# INLINE classNames #-}

classNamesOfType :: ClassNamer Type
classNamesOfType = _ReferenceType . classNamesOfReferenceType

instance HasClassNames JType where
  classNames = classNamesOfJType
  {-# INLINE classNames #-}

classNamesOfJType :: ClassNamer JType
classNamesOfJType = _JTRef . classNamesOfJRefType

instance HasClassNames JRefType where
  classNames = classNamesOfJRefType
  {-# INLINE classNames #-}

classNamesOfJRefType :: ClassNamer JRefType
classNamesOfJRefType = fold [_JTClass, _JTArray . classNamesOfJType]

instance HasClassNames ReferenceType where
  classNames = classNamesOfReferenceType
  {-# INLINE classNames #-}

classNamesOfReferenceType :: ClassNamer ReferenceType
classNamesOfReferenceType = fold
  [ _RefClassType . classNamesOfClassType
  , _RefTypeVariable . classNamesOfTypeVariable
  , _RefArrayType . classNamesOfArrayType
  ]

instance HasClassNames TypeVariable where
  classNames = classNamesOfTypeVariable
  {-# INLINE classNames #-}

classNamesOfTypeVariable :: ClassNamer TypeVariable
classNamesOfTypeVariable = typeVariableBound

instance HasClassNames ArrayType where
  classNames = classNamesOfArrayType
  {-# INLINE classNames #-}

classNamesOfArrayType :: ClassNamer ArrayType
classNamesOfArrayType = arrayType . classNamesOfAnnotated classNamesOfType


instance HasClassNames TypeArgument where
  classNames = classNamesOfTypeArgument
  {-# INLINE classNames #-}

classNamesOfTypeArgument :: ClassNamer TypeArgument
classNamesOfTypeArgument = fold
  [ _ExtendedTypeArg . classNamesOfAnnotated classNamesOfReferenceType
  , _ImplementedTypeArg . classNamesOfAnnotated classNamesOfReferenceType
  , _TypeArg . classNamesOfReferenceType
  ]
