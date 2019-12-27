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
import           Jvmhs.Data.Annotation
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
  , _classTypeParameters   :: [ TypeParameter ]
  -- ^ the type parameters of the class
  , _classSuper            :: Maybe ClassType
  -- ^ the superclass of the class
  , _classInterfaces       :: [ ClassType ]
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
  , _fieldType        :: ! Type
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
  , _methodParameters     :: ! [ Type ]
  -- ^ the method parameters
  , _methodReturnType     :: ! (Maybe Type)
  -- ^ the method return type
  , _methodTypeParameters :: ! [ TypeParameter ]
  -- ^ a method can have specific type parameters
  , _methodAccessFlags    :: ! (Set.Set MAccessFlag)
  -- ^ the set of access flags
  , _methodCode           :: ! (Maybe Code)
  -- ^ optionally the method can contain code
  , _methodExceptions     :: ! [ ThrowsSignature]
  -- ^ the method can have one or more exceptions
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

-- | A Class has a ClassName
instance HasClassName Class where
  className = className'
  {-# INLINE className #-}

-- | A Field has a 'FieldId'
instance HasFieldId Field where
  fieldId = undefined --  NameAndType (f ^. fieldName) (f ^. fieldType)
  {-# INLINE fieldId #-}

-- | A Methhi has a 'MethodId'
instance HasMethodId Method where
  methodId = undefined -- 
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


-- $(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 7} ''Method)
-- $(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''Field)
-- $(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''Class)
-- $(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''InnerClass)

-- $(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 6} ''Field)
-- $(deriveToJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 7} ''Method)


-- traverseClass ::
--   Traversal' ClassName a
--   -> Traversal' (Maybe ClassType) a
--   -> Traversal' (Set.Set CAccessFlag) a
--   -> Traversal' [ TypeParameter ] a
--   -> Traversal' [ ClassType ] a
--   -> Traversal' [ Field ] a
--   -> Traversal' [ Method ] a
--   -> Traversal' [ BootstrapMethod ] a
--   -> Traversal' (Maybe (ClassName, Maybe MethodId)) a
--   -> Traversal' [ InnerClass ] a
--   -> Traversal' Annotations a
--   -> Traversal' Class a
-- traverseClass tcn tsn taf ttp tis tfs tms tbs tem tin tano g s =
--   mkClass
--   <$> (tcn g . view className $ s)
--   <*>
--   ( ClassContent
--     <$> (taf g . view classAccessFlags $ s)
--     <*> (ttp g . view classTypeParameters $ s)
--     <*> (tsn g . view classSuper $ s)
--     <*> (tis g . view classInterfaces $ s)
--     <*> (tfs g . view classFields $ s)
--     <*> (tms g . view classMethods $ s)
--     <*> (tbs g . view classBootstrapMethods $ s)
--     <*> (tem g . view classEnclosingMethod $ s)
--     <*> (tin g . view classInnerClasses $ s)
--     <*> (tano g . view classAnnotations $ s)
--     <*> pure (view classVersion s)
--   )
-- {-# INLINE traverseClass #-}
--
-- traverseField ::
--      Traversal' Text.Text a
--   -> Traversal' FieldDescriptor a
--   -> Traversal' (Set.Set FAccessFlag) a
--   -> Traversal' (Maybe JValue) a
--   -> Traversal' (Maybe FieldSignature) a
--   -> Traversal' Annotations a
--   -> Traversal' Field a
-- traverseField tfn tfd taf tjv ts tano g s =
--   mkField
--   <$> (
--   (<:>)
--     <$> (tfn g . view fieldName $ s)
--     <*> (tfd g . view fieldDescriptor $ s)
--   ) <*> (
--   FieldContent
--     <$> (taf g . view fieldAccessFlags $ s)
--     <*> (tjv g . view fieldValue $ s)
--     <*> (ts g .  view fieldSignature $ s)
--     <*> (tano g .  view fieldAnnotations $ s)
--   )
-- {-# INLINE traverseField #-}
--
-- traverseMethod ::
--      Traversal' Text.Text a
--   -> Traversal' MethodDescriptor a
--   -> Traversal' (Set.Set MAccessFlag) a
--   -> Traversal' (Maybe Code) a
--   -> Traversal' [ ThrowsSignature ] a
--   -> Traversal' [ TypeParameter ] a
--   -> Traversal' [ TypeSignature ] a
--   -> Traversal' (Maybe TypeSignature) a
--   -> Traversal' Annotations a
--   -> Traversal' Method a
-- traverseMethod tfn tfd taf tc tex tp targ tret tano g s =
--   mkMethod
--   <$> (
--     (<:>)
--     <$> (tfn g . view methodName $ s)
--     <*> (tfd g . view methodDescriptor $ s)
--   ) <*> (
--     MethodContent
--       <$> (taf g . view methodAccessFlags $ s)
--       <*> (tc  g . view methodCode $ s)
--       <*> (tex g . view methodExceptions $ s)
--       <*> (tp  g . view methodTypeParameters $ s)
--       <*> (targ  g . view methodArguments $ s)
--       <*> (tret  g . view methodReturn $ s)
--       <*> (tano  g . view methodAnnotations $ s)
--     )
-- {-# INLINE traverseMethod #-}
