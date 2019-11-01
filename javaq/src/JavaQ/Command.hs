{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ApplicativeDo             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE ViewPatterns              #-}
{-|
Module      : JavaQ.Command
Description : A command is one of many queries
Copyright   : (c) Christian Kalhauge <kalhauge@cs.ucla.edu>
License     : BSD3
Maintainer  : kalhauge@cs.ucla.edu

A command is one of many queries
-}
module JavaQ.Command where

-- hexstring
import           Data.HexString

-- bytestring
import qualified Data.ByteString.Lazy.Char8   as BL

-- lens
import           Control.Lens                 hiding (argument, (.=))

-- aeson
import qualified Data.Aeson as Json

-- text
import qualified Data.Text                    as Text

-- cassava
import qualified Data.Csv                     as Csv

-- ansi-wl-pprint
import qualified Text.PrettyPrint.ANSI.Leijen as D

-- jvmhs
import           Jvmhs

-- mtl
import Control.Monad.Reader

data Format a where
  Txt :: (a -> Text.Text) -> Format a
  Csv :: Csv.Header -> (a -> [Csv.Record]) -> Format a
  Json :: Json.ToJSON b => (a -> b) -> Format a

formatName :: Format a -> Text.Text
formatName = \case
  Txt _ -> "txt"
  Csv _ _ -> "csv"
  Json _ -> "json"

data CommandConfig = CommandConfig
  { _cfgClassPath :: ![FilePath]
  , _cfgJre       :: !FilePath
  , _cfgUseStdlib :: !Bool
  } deriving (Show)

makeClassy ''CommandConfig

-- | Create a class loader from the config
createClassLoader ::
  ( HasCommandConfig env
  , MonadReader env m
  , MonadIO m)
  => m ClassLoader
createClassLoader =
  view cfgUseStdlib >>= \case
    True ->
      ( fromJreFolder
        <$> view cfgClassPath
        <*> view cfgJre
      ) >>= liftIO
    False ->
     ClassLoader [] [] <$> view cfgClassPath

data CommandType a where
  -- Preprocess ::  -> (b -> CommandType a) -> CommandType a

  Stream ::
    Granularity x
    -> ReaderT CommandConfig IO (x -> a)
    -> CommandType a

  Accumulator ::
    Granularity x
    -> ReaderT CommandConfig IO (x -> m)
    -> a
    -> (a -> m -> a)
    -> CommandType a

  Algorithm ::
    (forall m c. (HasCommandConfig c, MonadIO m, MonadReader c m, MonadClassPool m) => m a)
    -> CommandType a

data Granularity a where
  ClassNames :: Granularity (ClassName, ClassContainer)
  ClassFiles :: Granularity (ClassName, ClassContainer, BL.ByteString)
  Classes    :: Granularity Class
  Methods    :: Granularity (ClassName, Method)
  Fields     :: Granularity (ClassName, Field)

-- | The `Command` is about how to query the code.
data CommandSpec = forall a. CommandSpec
  { commandSpecName        :: Text.Text
  , commandSpecDescription :: D.Doc
  , commandSpecFormats     :: [Format a]
  , commandSpecType        :: CommandType a
  }

data Command = forall a. Command
  { commandName   :: Text.Text
  , commandFormat :: Format a
  , commandType   :: CommandType a
  }

mkCommand :: Text.Text -> Format a -> CommandType a -> Command
mkCommand name fmt cmdType = Command
  (name <> "+" <> formatName fmt)
  fmt
  cmdType

instance Show Command where
  show x = "Command { commandName = " ++ show (commandName x) ++ " , ...}"

doc :: Text.Text -> D.Doc
doc = D.text . Text.unpack

instance Csv.ToField ClassName where
  toField = Csv.toField . view fullyQualifiedName

instance Csv.ToField MethodId where
  toField = Csv.toField . serialize

instance Csv.ToField FieldId where
  toField = Csv.toField . serialize

instance Csv.ToField ClassContainer where
  toField = Csv.toField . classContainerFilePath

instance Csv.ToField HexString where
  toField = Csv.toField . toText
