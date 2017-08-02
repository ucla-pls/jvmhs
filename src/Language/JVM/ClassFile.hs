module Language.JVM.ClassFile
  ( ClassFile(..)
  ) where

-- Information from http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4
-- And from https://en.wikipedia.org/wiki/Java_class_file#cite_note-jvms-4.4-4


import           Data.Word

import           Language.JVM.Attribute (Attribute)
import           Language.JVM.Constant  (Constant, ConstantRef)
import           Language.JVM.Field     (Field)
import           Language.JVM.Method    (Method)

data ClassFile = ClassFile
  { magicNumber       :: !Word32
  , minorVersion      :: !Word16
  , majorVersion      :: !Word16
  , constantPoolCount :: !Word16
  , constantPool      :: [Constant]
  , accessFlags       :: !Word16
  , thisClass         :: !Word16
  , superClass        :: !Word16
  , interfacesCount   :: !Word16
  , interfaces        :: [ConstantRef]
  , fieldsCount       :: !Word16
  , fields            :: [Field]
  , methodsCount      :: !Word16
  , methods           :: [Method]
  , attributeCount    :: !Word16
  , attributes        :: [Attribute]
  }
