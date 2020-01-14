{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-|
Module      : Jvmhs.Data.Class
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD-3-Clause
Maintainer  : kalhauge@cs.ucla.edu

This module contains an syntaxtic interpretation of the class
file in `Language.JVM`.

To quickly access information about the class-file use the
`jvm-binary` package instead.

-}
module Jvmhs.Data.Class
  ( -- * Class
    Class(..)

  -- ** Lenses
  , className
  , classTypeParameters
  , classSuper
  , classInterfaces
  , classFields
  , classMethods
  , classAccessFlags
  , classBootstrapMethods
  , classEnclosingMethod
  , classInnerClasses
  , classAnnotations
  , classVersion

  -- ** Helpers
  , isInterface
  , classField
  , classMethod
  , classAbsFieldIds
  , classAbsMethodIds

  -- * Field
  , Field(..)
  , fieldName
  , fieldType
  , fieldAccessFlags
  , fieldValue
  , fieldAnnotations

  -- * Method
  , Method(..)
  , methodName
  , methodParameters
  , methodReturnType
  , methodTypeParameters
  , methodAccessFlags
  , methodCode
  , methodExceptions
  , methodAnnotations

  -- ** Parameters
  , Parameter(..)
  , parameterNameAndFlags
  , parameterType
  , parameterAnnotations

  -- ** Accessors
  , methodDescriptor

  -- * InnerClass
  , InnerClass(..)
  , innerClass
  , innerOuterClass
  , innerClassName
  , innerAccessFlags

  -- * Re-exports
  , CAccessFlag(..)
  , FAccessFlag(..)
  , MAccessFlag(..)
  , PAccessFlag(..)
  , ICAccessFlag(..)
  , JValue(..)
  )
where

-- base
import           Data.Foldable                 as F
import           Data.Word
import           GHC.Generics                   ( Generic )

-- deep-seq
import           Control.DeepSeq

-- lens
import           Control.Lens            hiding ( (.=) )

-- text
import qualified Data.Text                     as Text

-- containers
import qualified Data.Set                      as Set

-- jvm-binary
import qualified Language.JVM                  as B

-- jvmhs
import           Jvmhs.Data.BootstrapMethod
import           Jvmhs.Data.Code
import           Jvmhs.Data.Type
import           Jvmhs.Data.Identifier

-- | This is the class
data Class = Class
  { _className'            :: ClassName
  -- ^ the description of the class
  , _classAccessFlags      :: Set.Set CAccessFlag
  -- ^ access flags of the class
  , _classTypeParameters   :: [ Annotated TypeParameter ]
  -- ^ the type parameters of the class
  , _classSuper            :: Maybe (Annotated ClassType)
  -- ^ the superclass of the class
  , _classInterfaces       :: [ Annotated ClassType ]
  -- ^ a list of interfaces implemented by the class
  , _classFields           :: [ Field ]
  -- ^ a list of fields
  , _classMethods          :: [ Method ]
  -- ^ a list of methods
  , _classBootstrapMethods :: [ BootstrapMethod ]
  -- ^ a list of bootstrap methods. #TODO more info here
  , _classEnclosingMethod  :: Maybe (ClassName, Maybe MethodId)
  -- ^ maybe an enclosing class and method
  , _classInnerClasses     :: [ InnerClass ]
  -- ^ a list of inner classes
  , _classAnnotations      :: Annotations
  -- ^ the annotations of the class
  , _classVersion          :: Maybe (Word16, Word16)
  -- ^ the version of the class file
  } deriving (Eq, Show, Generic, NFData)

-- | This is the field
data Field = Field
  { _fieldName        :: ! Text.Text
  -- ^ the name of the field
  , _fieldType        :: ! (Annotated Type)
  -- ^ the type of the field
  , _fieldAccessFlags :: ! (Set.Set FAccessFlag)
  -- ^ the set of access flags
  , _fieldValue       :: ! (Maybe JValue)
  -- ^ an optional value
  , _fieldAnnotations :: Annotations
  -- ^ the annotations of the feild
  } deriving (Eq, Show, Generic, NFData)

-- | This is the method
data Method = Method
  { _methodName           :: ! Text.Text
  -- ^ the name of the method
  , _methodTypeParameters :: ! [ Annotated TypeParameter ]
  -- ^ a method can have specific type parameters
  , _methodParameters     :: ! [Parameter]
  -- ^ the method parameters
  , _methodReturnType     :: ! (Annotated ReturnType)
  -- ^ the method return type
  , _methodAccessFlags    :: ! (Set.Set MAccessFlag)
  -- ^ the set of access flags
  , _methodCode           :: ! (Maybe Code)
  -- ^ optionally the method can contain code
  , _methodExceptions     :: ! [ Annotated ThrowsType ]
  -- ^ the method can have one or more exceptions
  , _methodAnnotations     :: ! Annotations
  -- ^ the method can have annotations, these might be duplicated in the
  -- annotation for the return type.
  } deriving (Eq, Show, Generic, NFData)

-- | A parameter
data Parameter = Parameter
  { _parameterNameAndFlags :: ! (Maybe (Text.Text, Set.Set PAccessFlag))
  , _parameterType :: ! (Annotated Type)
  , _parameterAnnotations :: ! (Annotations)
  } deriving (Eq, Show, Generic, NFData)

-- | An inner class
data InnerClass = InnerClass
  { _innerClass       :: ClassName
  -- ^ The name of the inner class
  , _innerOuterClass  :: Maybe ClassName
  -- ^ The name of the other class.
  , _innerClassName   :: Maybe Text.Text
  -- ^ The name of the inner class, as to be prependend to the outer class.
  , _innerAccessFlags :: Set.Set ICAccessFlag
  -- ^ The access flags of the inner class.
  } deriving (Eq, Show, Generic, NFData)

makeLenses ''InnerClass

makeLenses ''Class
makeLenses ''Field
makeLenses ''Method
makeLenses ''Parameter

-- | A Class has a ClassName
instance HasClassName Class where
  className = className'
  {-# INLINE className #-}

-- | A Field has a 'FieldId'
instance HasFieldId Field where
  fieldId = to (\f -> mkFieldId (f ^. fieldName) (f ^. fieldDescriptor))
  {-# INLINE fieldId #-}

-- | A Method has a 'MethodId'
instance HasMethodId Method where
  methodId = to (\m -> mkMethodId (m ^. methodName) (m ^. methodDescriptor)) -- 
  {-# INLINE methodId #-}

-- | Fold over the absolute method ids of the class
classAbsMethodIds :: Fold Class AbsMethodId
classAbsMethodIds =
  (selfIndex <. classMethods . folded) . withIndex . to (uncurry mkAbsMethodId)

-- | Fold over the absoulte fieldIds of the class
classAbsFieldIds :: Fold Class AbsFieldId
classAbsFieldIds =
  (selfIndex <. classFields . folded) . withIndex . to (uncurry mkAbsFieldId)

-- | Choose a field in a class by a 'FieldId'
classField :: FieldId -> Getter Class (Maybe Field)
classField fid = classFields . to (find (\a -> a ^. fieldId == fid))

-- | Choose a method in a class by a 'MethodId'
classMethod :: MethodId -> Getter Class (Maybe Method)
classMethod mid = classMethods . to (find (\a -> a ^. methodId == mid))

-- | Check if a class is an interface
isInterface :: Class -> Bool
isInterface cls = B.CInterface `Set.member` (cls ^. classAccessFlags)

methodDescriptor :: Getter Method MethodDescriptor
methodDescriptor = to
  (\m -> MethodDescriptor
    (   m
    ^.. methodParameters
    .   folded
    .   parameterType
    .   annotatedContent
    .   to toBoundJType
    )
    (m ^. methodReturnType . annotatedContent . returnType . to
      (ReturnDescriptor . fmap toBoundJType)
    )
  )

fieldDescriptor :: Getter Field FieldDescriptor
fieldDescriptor =
  fieldType . annotatedContent . to (FieldDescriptor . toBoundJType)
