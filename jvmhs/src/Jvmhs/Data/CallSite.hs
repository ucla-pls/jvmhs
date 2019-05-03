{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE MultiParamTypeClasses        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-|
Module      : Jvmhs.Data.CallSite
Description : A description of a call-site
Copyright   : (c) Christian Kalhauge <kalhauge@cs.ucla.edu>
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

A description of a call-site
-}
module Jvmhs.Data.CallSite where

-- base
import           Data.Word
import           GHC.Generics                            (Generic)

-- deep-seq
import           Control.DeepSeq

import Jvmhs.Data.Code
import Jvmhs.Data.Named
import Jvmhs.Data.Type

data CallSiteName = CallSiteName
  { _callSiteMethod :: MethodName
  , _callSiteOffset :: !Word16
  } deriving (Show, Eq, Generic, NFData)

newtype CallSite = CallSite (Named CallSiteName CallSiteContent)
  deriving (Show, Eq, Generic)
  deriving anyclass (NFData)

-- instance HasName CallSiteName CallSite where
--   name = _Wrapped . name

data CallSiteContent = CallSiteContent
  -- { _callSiteInstruction :: ByteCode
  -- }
  deriving (Show, Eq, Generic, NFData)
