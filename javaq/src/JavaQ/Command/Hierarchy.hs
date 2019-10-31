{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
module JavaQ.Command.Hierarchy where

-- base
import           Data.Maybe
import           Data.Foldable
import qualified Data.List as List

-- unordered
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet        as Set

-- aeson
import           Data.Aeson
import           Data.Aeson.TH

-- lens
import           Control.Lens        hiding (argument, (.=))

-- jvmhs
import           Jvmhs

-- javaq
import           JavaQ.Command

newtype HR = HR { getHierarchy :: HashMap.HashMap ClassName HRInfo }
  deriving (Show, Eq)

emptyHR :: HR
emptyHR = HR HashMap.empty

instance ToJSON HR where
  toJSON (HR a) = toJSON a

instance FromJSON HR where
  parseJSON v = HR <$> parseJSON v

data HRInfo = HRInfo
  { _hrExtendedBy    :: (Set.HashSet ClassName)
  , _hrSuperclasses  :: [ClassName]
  , _hrImplements    :: (Set.HashSet ClassName)
  , _hrImplementedBy :: (Set.HashSet ClassName)
  , _hrIsInterface   :: (Bool)
  } deriving (Show, Eq)

makeLenses ''HRInfo

emptyHRInfo :: HRInfo
emptyHRInfo =
  HRInfo Set.empty [] Set.empty Set.empty False

addNode :: HR -> Class -> HR
addNode (HR hm) cls = HR hm'
  where
    hm' =
      HashMap.insert (cls ^. className) hi
      . updatedSuperclasses
      . updatedImplementedByAbove
      . updatedBelow
      $ hm

    hi = getOrEmpty (cls ^. className)
      & hrSuperclasses .~ _superclasses
      & hrImplements .~ _interfaces
      & hrIsInterface .~ isInterface cls

    -- Creates list of superclasses for node class
    _superclasses =
      cls ^.. classSuper._Just.classTypeName.(id <> lookupCls.hrSuperclasses.folded)

    -- get all interfaces and then parent interfaces' interfaces
    _interfaces =
      cls ^. classInterfaces.folded.classTypeName.(to Set.singleton <> lookupCls.hrImplements)

    extendedBy = Set.singleton (cls ^. className) <> hi ^. hrExtendedBy
    implementedBy = Set.singleton (cls ^. className) <> hi ^. hrImplementedBy

    -- update all classes above me
    updatedSuperclasses =
      foreachKey _superclasses $ \i ->
        i & hrExtendedBy <>~ extendedBy

    updatedImplementedByAbove =
      foreachKey _interfaces $ \i ->
        i & hrImplementedBy <>~ extendedBy <> implementedBy

    -- update all classes lower than me
    updatedBelow =
      foreachKey (hi ^. (hrExtendedBy <> hrImplementedBy)) $ \i ->
        i & hrImplements <>~ _interfaces
          & hrSuperclasses %~
          (\case
              l | List.elem (cls ^. className) l -> l ++ _superclasses
              l -> l
          )

    foreachKey ::
      Foldable f
      => f ClassName
      -> (HRInfo -> HRInfo)
      -> HashMap.HashMap ClassName HRInfo
      -> HashMap.HashMap ClassName HRInfo
    foreachKey f fn hm'' =
      foldl' (flip $ HashMap.alter (Just . fn . fromMaybe emptyHRInfo)) hm'' f

    getOrEmpty cn = fromMaybe emptyHRInfo $ HashMap.lookup cn hm

    lookupCls :: Getter ClassName HRInfo
    lookupCls = to getOrEmpty


$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 3}''HRInfo)

hierarchyCmd :: CommandSpec
hierarchyCmd = CommandSpec
  "hierarchy"
  "Get information about the hierarchy."
  [ Json id ]
  $ Accumulator
  (Classes id)
  emptyHR
  addNode
