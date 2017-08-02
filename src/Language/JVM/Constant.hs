module Language.JVM.Constant
  ( Constant (..)
  , ConstantRef
  ) where

import qualified Data.Text as Text
import Data.Word

type ConstantRef = Word16

data Constant
  = String Text.Text
  | Integer !Word32
  | Float !Word32
  | Long !Word64
  | Double !Word64
  | ClassRef !ConstantRef
  | StringRef !ConstantRef
  | FieldRef ConstantRef ConstantRef
  | MethodRef ConstantRef ConstantRef
  | InterfaceMethodRef ConstantRef ConstantRef
  | NameAndType ConstantRef ConstantRef
  | MethodHandle Word8 ConstantRef
  | MethodType ConstantRef
  | InvokeDynamic ConstantRef ConstantRef
