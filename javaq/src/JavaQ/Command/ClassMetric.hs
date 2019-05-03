{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE DeriveGeneric             #-}
{-|
Module      : JavaQ.Command.ClassMetric
Description : ClassMetric
Copyright   : (c) Christian Kalhauge <kalhauge@cs.ucla.edu>
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

-}
module JavaQ.Command.ClassMetric where

-- base
import           Data.Word
import           GHC.Generics                 (Generic)

-- hexstring
import           Data.HexString

-- cryptohash-sha256
import           Crypto.Hash.SHA256           as SHA256

-- lens
import           Control.Lens                 hiding (argument, (.=))

-- text
import qualified Data.Text                    as Text

-- aeson
import qualified Data.Aeson                   as Json
import qualified Data.Aeson.TH                as Json

-- cassava
import qualified Data.Csv                     as Csv

-- vector
import qualified Data.Vector                  as V

-- jvmhs
import           Jvmhs
import           Jvmhs.Data.Code

-- javaq
import JavaQ.Command

data ClassMetric = ClassMetric
  { cmName         :: !ClassName
  , cmSize         :: !Word64
  , cmSha256       :: !HexString
  , cmMethods      :: !Int
  , cmFields       :: !Int
  , cmInstructions :: !Int
  , cmErrors       :: !Int
  , cmErrorMessage :: !String
  } deriving (Show, Eq, Generic)

$(Json.deriveToJSON Json.defaultOptions{Json.fieldLabelModifier = Json.camelTo2 '_' . drop 2} ''ClassMetric)

instance Csv.ToRecord ClassMetric where
  toRecord = Csv.genericToRecord (Csv.defaultOptions { Csv.fieldLabelModifier = Json.camelTo2 '_' . drop 2 })

instance Csv.ToNamedRecord ClassMetric where
  toNamedRecord = Csv.genericToNamedRecord (Csv.defaultOptions { Csv.fieldLabelModifier = Json.camelTo2 '_' . drop 2 })

instance Csv.DefaultOrdered ClassMetric where
  headerOrder = Csv.genericHeaderOrder (Csv.defaultOptions { Csv.fieldLabelModifier = Json.camelTo2 '_' . drop 2 })

classmetricsCmd :: CommandSpec
classmetricsCmd = CommandSpec "class-metrics"
  "A stream of metrics for the classes."
  [ Json id
  , Csv (Csv.headerOrder (undefined :: ClassMetric)) ((:[]) . Csv.toRecord)
  ]
  . Stream $ ClassBytes fn
  where
    fn (cn, bts) =
      let (hsh, ln) = SHA256.hashlazyAndLength bts
      in case readClassFile' True bts of
        Right clsf ->
          let cls = convertClass clsf
          in ClassMetric
            { cmName = cn
            , cmSize = ln
            , cmSha256 = fromBytes hsh
            , cmFields = cls ^. classFieldList.to length
            , cmMethods = cls ^. classMethodList.to length
            , cmInstructions =
                sumOf (classMethods.folded.methodCode._Just.codeByteCode.to V.length) cls
            , cmErrors = if cn /= cls ^.className then 1 else 0
            , cmErrorMessage =
                if cn /= cls ^.className
                then "Expected other classname: " ++ Text.unpack (cls ^.className.fullyQualifiedName)
                else ""
            }
        Left msg ->
          ClassMetric
            { cmName = cn
            , cmSize = ln
            , cmSha256 = fromBytes hsh
            , cmFields = 0
            , cmMethods = 0
            , cmInstructions = 0
            , cmErrors = 1
            , cmErrorMessage = show msg
            }
