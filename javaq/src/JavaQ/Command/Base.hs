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

-- javaq
import JavaQ.Command

listClassesCmd :: CommandSpec
listClassesCmd = CommandSpec "list-classes"
  "A stream of class names."
  [ Txt (view fullyQualifiedName)
  , Csv (Csv.header ["name"]) (\a -> [Csv.record [ Csv.toField a ]])
  ]
  $ Stream ClassNames (return fst)

listMethodsCmd :: CommandSpec
listMethodsCmd = CommandSpec
  "list-methods"
  "A stream of method names."
  [ Txt serialize
  , Csv (Csv.header ["class", "name"]) ((:[]) . Csv.toRecord . (\m -> (m^.className, m^.methodId)))
  ]
  $ Stream Methods (return $ uncurry mkAbsMethodId)

listFieldsCmd :: CommandSpec
listFieldsCmd = CommandSpec
  "list-fields"
  "A stream of field names."
  [ Txt serialize
  , Csv (Csv.header ["class", "name"]) ((:[]) . Csv.toRecord . (\f -> (f^.className, f^.fieldId)) )
  ]
  $ Stream  Fields (return $ uncurry mkAbsFieldId)


containersCmd :: CommandSpec
containersCmd = CommandSpec "containers"
  "A stream of class names and their containers."
  [ Txt (\(a, c) -> serialize a <> "\t" <> Text.pack (classContainerFilePath c))
  , Csv (Csv.header ["name", "container"]) (\a -> [Csv.toRecord a])
  ]
  $ Stream ClassNames (return id)

decompileCmd :: CommandSpec
decompileCmd = CommandSpec "decompile"
  "A stream of decompiled classes."
  [ Json id
  ]
  $ Stream Classes (return id)
