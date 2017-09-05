{-# LANGUAGE TemplateHaskell   #-}
module Language.JVM.Class
  ( Class (..)

  , name
  , fields
  , interfaces
  , methods
  , super

  , fromBinary
  , decodeClassOrFail
  ) where

import qualified Data.ByteString.Lazy          as BL

-- import           Data.Aeson
-- import           Data.Aeson.TH
import          Control.Monad

import qualified Language.JVM.Binary.ClassFile as B
import qualified Language.JVM.Binary.Constant  as C
import           Language.JVM.ClassName
import           Language.JVM.Binary.SizedList
import qualified Language.JVM.Field            as Field
import qualified Language.JVM.Method           as Method

import Control.Lens

data Class = Class
  { _name       :: ClassName
  , _super      :: ClassName
  , _interfaces :: [ ClassName ]
  , _fields     :: [ Field.Field ]
  , _methods    :: [ Method.Method ]
  } deriving (Eq, Show)

fromBinary :: B.ClassFile -> Maybe Class
fromBinary clsf = do
  let cp = B.constantPool clsf
  _name <- C.lookupClassName (B.thisClass clsf) cp
  _super <- C.lookupClassName (B.superClass clsf) cp

  _interfaces <-
    forM (unSizedList16 $ B.interfaces clsf) (flip C.lookupClassName cp)

  _fields <-
    forM (unSizedList16 $ B.fields clsf) $ Field.fromBinary cp

  _methods <-
    forM (unSizedList16 $ B.methods clsf) $ Method.fromBinary cp

  return $ Class _name _super _interfaces _fields _methods

makeLenses ''Class

decodeClassOrFail :: BL.ByteString -> Either String Class
decodeClassOrFail bs = do
  case B.decodeClassFileOrFail bs of
    Right cf ->
      case fromBinary cf of
        Just cl -> Right cl
        Nothing -> Left "Broken class-file format"
    Left msg ->
      Left $ "Could not decode class-file: " ++ msg

--deriveJSON (defaultOptions { fieldLabelModifier = drop 1}) ''Class
