{-# LANGUAGE OverloadedStrings         #-}
{-|
Module      : JavaQ.Command.Base
Description : Base
Copyright   : (c) Christian Kalhauge <kalhauge@cs.ucla.edu>
License     : BSD3
Maintainer  : kalhauge@cs.ucla.edu

Base
-}
module JavaQ.Command.Base where

-- lens
import           Control.Lens                 hiding (argument, (.=))

-- text
import qualified Data.Text                    as Text

-- cassava
import qualified Data.Csv                     as Csv

-- jvmhs
import           Jvmhs
import           Jvmhs.Data.Named

-- javaq
import JavaQ.Command

listClassesCmd :: CommandSpec
listClassesCmd = CommandSpec "list-classes"
  "A stream of class names."
  [ Txt (view fullyQualifiedName)
  , Csv (Csv.header ["name"]) (\a -> [Csv.record [ Csv.toField a ]])
  ]
  . Stream $ ClassNames const

listMethodsCmd :: CommandSpec
listMethodsCmd = CommandSpec
  "list-methods"
  "A stream of method names."
  [ Txt (\(c, m) -> view fullyQualifiedName c <> "." <> methodNameToText m)
  , Csv (Csv.header ["class", "name"]) ((:[]) . Csv.toRecord)
  ]
  . Stream $ Methods (\cn m -> (cn, m ^. name))

listFieldsCmd :: CommandSpec
listFieldsCmd = CommandSpec
  "list-fields"
  "A stream of field names."
  [ Txt (\(c, f) -> view fullyQualifiedName c <> "." <> fieldNameToText f)
  , Csv (Csv.header ["class", "name"]) ((:[]) . Csv.toRecord)
  ]
  . Stream $ Fields (\cn f -> (cn, f ^. name))


containersCmd :: CommandSpec
containersCmd = CommandSpec "containers"
  "A stream of class names and their containers."
  [ Txt (\(a, c) -> view fullyQualifiedName a <> "\t" <> Text.pack (classContainerFilePath c))
  , Csv (Csv.header ["name", "container"]) (\a -> [Csv.toRecord a])
  ]
  . Stream
  $ ClassNames (,)

decompileCmd :: CommandSpec
decompileCmd = CommandSpec "decompile"
  "A stream of decompiled classes."
  [ Json id
  ]
  . Stream
  $ Classes id
