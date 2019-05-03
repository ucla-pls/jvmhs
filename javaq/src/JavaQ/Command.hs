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
License     : MIT
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

-- text
import qualified Data.Text                    as Text

-- cassava
import qualified Data.Csv                     as Csv

-- ansi-wl-pprint
import qualified Text.PrettyPrint.ANSI.Leijen as D

-- jvmhs
import           Jvmhs

data Format a
  = Txt (a -> Text.Text)
  | Csv Csv.Header (a -> [Csv.Record])
  | Json (a -> BL.ByteString)

formatName :: Format a -> Text.Text
formatName = \case
  Txt _ -> "txt"
  Csv _ _ -> "csv"
  Json _ -> "json"

data CommandType a where
  Stream :: StreamFunction a -> CommandType a

data StreamFunction a
  = Containers ((ClassName, ClassContainer) -> a)
  | ClassBytes ((ClassName, BL.ByteString) -> a)
  | Classes (Class -> a)

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

instance Csv.ToField MethodName where
  toField = Csv.toField . methodNameToText

instance Csv.ToField FieldName where
  toField = Csv.toField . fieldNameToText

instance Csv.ToField ClassContainer where
  toField = Csv.toField . classContainerFilePath

instance Csv.ToField HexString where
  toField = Csv.toField . toText
