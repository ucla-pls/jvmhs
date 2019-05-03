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

-- aeson
import qualified Data.Aeson                   as Json

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

classnameCmd :: CommandSpec
classnameCmd = CommandSpec "list"
  "A stream of class names."
  [ Txt (view fullyQualifiedName)
  , Csv (Csv.header ["name"]) (\a -> [Csv.record [ Csv.toField a ]])
  ]
  . Stream
  $ Containers fst

containersCmd :: CommandSpec
containersCmd = CommandSpec "containers"
  "A stream of class names and their containers."
  [ Txt (\(a, c) -> view fullyQualifiedName a <> "\t" <> Text.pack (classContainerFilePath c))
  , Csv (Csv.header ["name", "container"]) (\a -> [Csv.toRecord a])
  ]
  . Stream
  $ Containers id

decompileCmd :: CommandSpec
decompileCmd = CommandSpec "decompile"
  "A stream of decompiled classes."
  [ Json Json.encode
  ]
  . Stream
  $ Classes id

instance Csv.ToField ClassName where
  toField = Csv.toField . view fullyQualifiedName

instance Csv.ToField ClassContainer where
  toField = Csv.toField . classContainerFilePath

instance Csv.ToField HexString where
  toField = Csv.toField . toText
