{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Jvmhs.Format.ClassFile.Shared
Copyright   : (c) Christian Gram Kalhauge, 2019
License     : BSD3
Maintainer  : kalhauge@cs.ucla.edu
-}
module Jvmhs.Format.ClassFile.Shared where

-- base
import Control.Category
import Prelude hiding (
  id,
  (.),
 )

-- text
import qualified Data.Text as Text

-- jvm-binary
import qualified Language.JVM as B

import Jvmhs.Format.Internal

type FormatError = String
type Formatter = PartIso [FormatError]

textSerialize :: B.TextSerializable a => Formatter Text.Text a
textSerialize = PartIso (validateEither . B.deserialize) (pure . B.serialize)
