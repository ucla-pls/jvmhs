{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fno-warn-unused-imports #-}
module Jvmhs.Format.Json where

-- aeson
import           Data.Aeson

-- lens
import           Control.Lens

-- containers
import qualified Data.Set                      as Set

-- text
import qualified Data.Text                     as Text

-- jvmhs
import           Jvmhs.Data.Class
import           Jvmhs.Data.Identifier

{-

How do we format json?

-}

instance ToJSON Class where
  toJSON _ = String "class"
