{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-|
Module      : Jvmhs.Data.BootstrapMethod
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : BSD3
Maintainer  : kalhuage@cs.ucla.edu

This module works with the BootstrapMethod. This is a work in progress.
-}

module Jvmhs.Data.BootstrapMethod
  where

import           Control.DeepSeq
import           Data.Aeson
import           GHC.Generics
import qualified Language.JVM                            as B
import qualified Language.JVM.Attribute.BootstrapMethods as B

newtype BootstrapMethod = BootstrapMethod
  { _unBootstrapMethod :: B.BootstrapMethod B.High
  }
  deriving (Show, Eq, Generic)
  deriving anyclass NFData

fromBinaryBootstrapMethod :: B.BootstrapMethod B.High -> BootstrapMethod
fromBinaryBootstrapMethod =
  BootstrapMethod

toBinaryBootstrapMethod :: BootstrapMethod -> B.BootstrapMethod B.High
toBinaryBootstrapMethod =
  _unBootstrapMethod

instance ToJSON BootstrapMethod where
    toJSON _ = String $ "<bootstrapmethod>"
