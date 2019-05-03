{-# LANGUAGE OverloadedStrings         #-}
{-|
Module      : JavaQ.Command.Base
Description : Base
Copyright   : (c) Christian Kalhauge <kalhauge@cs.ucla.edu>
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

Base
-}
module JavaQ.Command.Base where

-- aeson
import qualified Data.Aeson                   as Json

-- lens
import           Control.Lens                 hiding (argument, (.=))

-- text
import qualified Data.Text                    as Text

-- cassava
import qualified Data.Csv                     as Csv

-- jvmhs
import           Jvmhs

-- javaq
import JavaQ.Command

listClassesCmd :: CommandSpec
listClassesCmd = CommandSpec "list-classes"
  "A stream of class names."
  [ Txt (view fullyQualifiedName)
  , Csv (Csv.header ["name"]) (\a -> [Csv.record [ Csv.toField a ]])
  ]
  . Stream
  $ Containers fst

listMethodsCmd :: CommandSpec
listMethodsCmd = CommandSpec
  "list-methods"
  "A stream of method names."
  [ Txt ( Text.intercalate "\n"
          . map (\(c, m) -> view fullyQualifiedName c <> "." <> methodNameToText m :: Text.Text)
        )
  , Csv (Csv.header ["class", "name"]) (map Csv.toRecord)
  ]
  (Stream $ Classes (toListOf classAbsMethodNames))

listFieldsCmd :: CommandSpec
listFieldsCmd = CommandSpec
  "list-fields"
  "A stream of field names."
  [ Txt ( Text.intercalate "\n"
          . map (\(c, m) -> view fullyQualifiedName c <> "." <> fieldNameToText m)
        )
  , Csv (Csv.header ["class", "name"]) (map Csv.toRecord)
  ]
  (Stream $ Classes (toListOf classAbsFieldNames))


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
