module Language.JVM.Field
  ( Field (..)
  , fromBinary
  ) where

import           Control.Monad                (forM)
import qualified Data.Set                     as S
import qualified Data.Vector                  as V

import qualified Language.JVM.Binary.Constant as C
import qualified Language.JVM.Binary.Field    as B

import qualified Data.Text                    as Text

type FieldName = Text.Text

data Field = Field
  { accessFlags :: B.AccessFlags
  , name        :: FieldName
  , descriptor  :: C.FieldDescriptor
  } deriving (Eq, Show)

fromBinary :: C.ConstantPool -> B.Field -> Maybe Field
fromBinary cp f = do
  name <- C.lookupText (B.nameIndex f) cp
  descriptor <- C.lookupFieldDescriptor (B.descriptorIndex f) cp
  return $ Field (B.accessFlags f) name descriptor
