{-# LANGUAGE TemplateHaskell   #-}
{-|
Module      : Jvmhs.Data.Class
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module contains an syntaxtic interpretation of the class
file in `Language.JVM`.

To quickly access information about the class-file use the
`jvm-binary` package instead.

-}
module Jvmhs.Data.Class
  ( -- * Data structures
    Class (..)
  , className
  , classSuper
  , classInterfaces
  , classFields
  , classMethods
  , classBootstrapMethods

  , Field (..)
  , fieldAccessFlags
  , fieldName
  , fieldDescriptor
  , fieldConstantValue

  -- * Helpers

  , checked

  -- * Rexports
  , module Language.JVM.Type
  ) where

import qualified Language.JVM as B
import Language.JVM.Type

import qualified Data.Set as Set
import qualified Data.Text as Text
import Control.Lens

-- This is the class
data Class = Class
  { _className :: ClassName
  -- ^ the name of the class
  , _classSuper :: ClassName
  -- ^ the name of the super class
  , _classInterfaces :: [ ClassName ]
  -- ^ a list of interfaces implemented by the class
  , _classFields :: [ Field ]
  -- ^ a list of fields
  , _classMethods :: [ Method ]
  -- ^ a list of methods
  , _classBootstrapMethods :: [ BootstrapMethod ]
  -- ^ a list of bootstrap methods. #TODO more info here
  } deriving (Eq, Show)

-- This is the field
data Field = Field
  { _fieldAccessFlags :: Set.Set B.FAccessFlag
  -- ^ the set of access flags
  , _fieldName :: Text.Text
  -- ^ the name of the field
  , _fieldDescriptor :: FieldDescriptor
  -- ^ the field type descriptor
  , _fieldConstantValue :: Maybe B.ConstantValue
  -- ^ an optional constant value
  } deriving (Eq, Show)

data Method = Method deriving (Eq, Show)
data BootstrapMethod = BootstrapMethod deriving (Eq, Show)

makeLenses ''Class
makeLenses ''Field

failWith :: String -> Maybe a -> Either String a
failWith s ma =
  case ma of
    Just a -> Right a
    Nothing -> Left s

checked :: Iso' B.ClassFile (Either String Class)
checked = iso fromBinary toBinary
  where
    fromBinary clsfile = do
      let c = B.cConstantPool clsfile
      cn <- failWith "Could not find 'this' class" $ B.cThisClass c clsfile
      cs <- failWith "Could not find 'super' class" $ B.cSuperClass c clsfile

      is <- failWith "Could not load 'interfaces'"
        . sequence . map (B.deref c)
        $ B.cInterfaces clsfile

      flds <- sequence $ clsfile ^.. to B.cFields . traverse . checkedField
      mths <- sequence $ clsfile ^.. to B.cMethods . traverse . checkedMethod

      let btms = []
      return $ Class cn cs is flds mths btms

    toBinary = undefined

checkedField :: Iso' B.Field (Either String Field)
checkedField = iso fromBinary toBinary
  where
    fromBinary = undefined
    toBinary = undefined

checkedMethod :: Iso' B.Method (Either String Method)
checkedMethod = iso fromBinary toBinary
  where
    fromBinary = undefined
    toBinary = undefined
