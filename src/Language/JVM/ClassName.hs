{-# LANGUAGE OverloadedStrings #-}
module Language.JVM.ClassName
  ( ClassName (..)
  , fromStr
  , fromDot
  , fromDotStr
  , pathOfClass
  ) where

import qualified Data.Text       as Text
import           System.FilePath (FilePath, (<.>), (</>))
import           Data.Aeson

newtype ClassName =
  ClassName Text.Text
  deriving (Eq, Show, Ord)

instance FromJSON ClassName where
  parseJSON = withText "the classname" $ \text ->
    return $ ClassName text

instance ToJSON ClassName where
  toJSON (ClassName text) =
    toJSON text

fromStr :: String -> ClassName
fromStr =
  ClassName . Text.pack

fromDot :: Text.Text -> ClassName
fromDot =
  ClassName . Text.replace "." "/"

fromDotStr :: String -> ClassName
fromDotStr =
  fromDot . Text.pack

pathOfClass
  :: FilePath
  -- ^ the source folder
  -> ClassName
  -- ^ the class name
  -> FilePath
pathOfClass fp (ClassName name) =
  fp </> Text.unpack name <.> "class"
