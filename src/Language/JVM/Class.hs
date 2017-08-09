{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.JVM.Class
  ( Class (..)

  , fromBinary
  , decodeClass
  , decodeClassOrFail

  , fromDot
  , fromDotS
  , pathOfClass
  , C.ClassName
  ) where

import GHC.Generics

import           System.FilePath               (FilePath, (<.>), (</>))

import           Control.Monad                 (forM)
import qualified Data.Set                      as S
import qualified Data.Vector                   as V

import qualified Language.JVM.Binary.ClassFile as B
import qualified Language.JVM.Binary.Constant  as C
import qualified Language.JVM.Field            as Field
import qualified Language.JVM.Method           as Method

import qualified Data.Text                     as Text


data Class = Class
  { name       :: C.ClassName
  , super      :: C.ClassName
  , interfaces :: S.Set C.ClassName
  , fields     :: V.Vector Field.Field
  , methods    :: V.Vector Method.Method
  } deriving (Eq, Show, Generic)


fromBinary :: B.ClassFile -> Maybe Class
fromBinary clsf = do
  let cp = B.constantPool clsf
  name <- C.lookupClassName (B.thisClass clsf) cp
  super <- C.lookupClassName (B.superClass clsf) cp


  interfaces <- S.fromList . V.toList <$>
    V.forM (B.interfaces clsf) (flip C.lookupClassName cp)

  fields <- V.forM (B.fields clsf) $ Field.fromBinary cp

  methods <- V.forM (B.methods clsf) $ Method.fromBinary cp

  return $ Class name super interfaces fields methods

fromDot :: Text.Text -> C.ClassName
fromDot =
  C.ClassName . Text.replace "." "/"

fromDotS :: String -> C.ClassName
fromDotS =
  fromDot . Text.pack

pathOfClass
  :: FilePath
  -- ^ the source folder
  -> C.ClassName
  -- ^ the class name
  -> FilePath
pathOfClass fp (C.ClassName name) =
  fp </> Text.unpack name <.> "class"

decodeClass :: FilePath -> IO Class
decodeClass fp = do
  cf <- B.decodeClassFile fp
  let Just cls = fromBinary cf
  return cls

decodeClassOrFail :: FilePath -> IO (Either String Class)
decodeClassOrFail fp = do
  res <- B.decodeClassFileOrFail fp
  return $ case res of
    Right cf ->
      case fromBinary cf of
        Just cl -> Right cl
        Nothing -> Left "Broken class-file format"
    Left msg -> Left $ "Could not decode class-file: " ++ msg
