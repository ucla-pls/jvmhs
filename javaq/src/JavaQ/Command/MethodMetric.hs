{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DeriveGeneric             #-}
{-|
Module      : JavaQ.Command.MethodMetric
Description : MethodMetrics
Copyright   : (c) Christian Kalhauge <kalhauge@cs.ucla.edu>
License     : BSD3
Maintainer  : kalhauge@cs.ucla.edu

MethodMetrics
-}
module JavaQ.Command.MethodMetric where

-- base
import GHC.Generics (Generic)

-- lens
import Control.Lens

-- aeson
import qualified Data.Aeson                   as Json
import qualified Data.Aeson.TH                as Json

-- cassava
import qualified Data.Csv                     as Csv

-- jvmhs
import Jvmhs
import Jvmhs.Data.Named

-- javaq
import JavaQ.Command

data MethodMetric = MethodMetric
  { mmClass :: ClassName
  , mmName :: MethodId
  } deriving (Show, Generic)

$(Json.deriveToJSON Json.defaultOptions{Json.fieldLabelModifier = Json.camelTo2 '_' . drop 2} ''MethodMetric)

instance Csv.ToRecord MethodMetric where
  toRecord = Csv.genericToRecord (Csv.defaultOptions { Csv.fieldLabelModifier = Json.camelTo2 '_' . drop 2 })

instance Csv.ToNamedRecord MethodMetric where
  toNamedRecord = Csv.genericToNamedRecord (Csv.defaultOptions { Csv.fieldLabelModifier = Json.camelTo2 '_' . drop 2 })

instance Csv.DefaultOrdered MethodMetric where
  headerOrder = Csv.genericHeaderOrder (Csv.defaultOptions { Csv.fieldLabelModifier = Json.camelTo2 '_' . drop 2 })


methodmetricCmd :: CommandSpec
methodmetricCmd = CommandSpec
  "method-metrics"
  "A stream of method metrics."
  [ Json id
  , Csv (Csv.headerOrder (undefined :: MethodMetric)) (map Csv.toRecord)
  ]
  (Stream $ Classes fn)
  where
    fn cls =
      [ MethodMetric (cls ^. className) mn
      | Method (Named mn _) <- cls ^. classMethods
      ]
