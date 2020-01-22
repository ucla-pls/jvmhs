{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE QuantifiedConstraints         #-}
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
  , reindexBootstrapMethods

  -- * Field
  , Field(..)
  , fieldName
  , fieldType
  , fieldAccessFlags
  , fieldValue
  , fieldAnnotations

  -- ** Accessors
  , fieldDescriptor

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

  -- ** Accessors
  , methodDescriptor

  -- ** Parameters
  , Parameter(..)
  , parameterNameAndFlags
  , parameterType
  , parameterVisible
  , parameterAnnotations

  -- * InnerClass
  , InnerClass(..)
  , innerClass
  , innerOuterClass
  , innerClassName
  , innerAccessFlags

  -- ** accessors
  , staticInnerClasses

  -- * BootstrapMethod 
  , BootstrapMethod(..)
  , bootstrapMethodHandle
  , bootstrapMethodArguments

  -- ** Re-exports
  , B.MethodHandle(..)
  , _MHField
  , _MHMethod
  , _MHInterface

  -- * Re-exports
  , CAccessFlag(..)
  , FAccessFlag(..)
  , MAccessFlag(..)
  , PAccessFlag(..)
  , ICAccessFlag(..)
  )
where

-- base
import           Data.Foldable                 as F
import           Data.Word
import           GHC.Generics                   ( Generic )

-- deep-seq
import           Control.DeepSeq

-- lens
import           Control.Lens
import           Data.Set.Lens                  ( setOf )

-- mtl
import           Control.Monad.State

-- text
import qualified Data.Text                     as Text

-- containers
import qualified Data.Set                      as Set
import qualified Data.IntMap.Strict            as IntMap

-- jvm-binary
import qualified Language.JVM                  as B

-- jvmhs
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
  , _classBootstrapMethods :: IntMap.IntMap BootstrapMethod
  -- ^ a map of bootstrap methods. Essentially, this is a list but the
  -- list might not be complete.
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
  -- ^ The name and access flags of the parameter as saved in the
  -- MethodParameter attribute.
  , _parameterVisible :: Bool
  -- ^ Some parameters are not visible, this means that they are displayed
  -- in the Method Signature but is still visible in the MethodDescription.
  , _parameterType :: ! (Annotated Type)
  -- ^ The type of the parameter, can have another (and the same)
  -- annotations as the parameter itself.
  , _parameterAnnotations :: ! (Annotations)
  -- ^ The anntoations of the parameter.
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

-- | A BootstrapMethod
data BootstrapMethod = BootstrapMethod
  { _bootstrapMethodHandle :: B.MethodHandle B.High
  , _bootstrapMethodArguments :: [JValue]
  } deriving (Eq, Show, Generic, NFData)

makeLenses ''Class
makeLenses ''Field
makeLenses ''Method
makeLenses ''Parameter
makeLenses ''InnerClass
makeLenses ''BootstrapMethod

makePrisms ''B.MethodHandle

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

-- | The descriptor of a method
methodDescriptor :: Getter Method MethodDescriptor
methodDescriptor = to
  (\m -> MethodDescriptor
    (m ^.. methodParameters . folded . parameterType . simpleType)
    (m ^. methodReturnType . simpleType . to ReturnDescriptor)
  )

-- | The descriptor of a field
fieldDescriptor :: Getter Field FieldDescriptor
fieldDescriptor = fieldType . simpleType . to FieldDescriptor

-- | The set of all static inner classes. This is important 
-- for assinging annotations
staticInnerClasses :: [InnerClass] -> Set.Set ClassName
staticInnerClasses = setOf
  (folded . filtered (view $ innerAccessFlags . contains ICStatic) . innerClass)


-- | BootstrapMethods are intented to be a list, but if some of the method
-- are created or removed the indicies in the code might not match.
-- This function takes a Class and reindex the bootstrapmethod list and
-- update the indicies in the code.
reindexBootstrapMethods :: Class -> ([BootstrapMethod], Class)
reindexBootstrapMethods = runState $ do
  (indicies, bootmethods) <-
    fmap unzip . use $ classBootstrapMethods . to IntMap.toAscList
  let reindexer = IntMap.fromAscList $ zip indicies [0 ..]

  classBootstrapMethods .= IntMap.fromAscList (zip [0 ..] bootmethods)

  classMethods
    .  traverse
    .  methodCode
    .  _Just
    .  codeByteCode
    .  traverse
    .  byteCodeOpcode
    %= \case
         B.Invoke (B.InvkDynamic (B.InvokeDynamic i m)) -> B.Invoke
           (B.InvkDynamic
             (B.InvokeDynamic
               (IntMap.findWithDefault i (fromIntegral i) reindexer)
               m
             )
           )
         a -> a

  return bootmethods
