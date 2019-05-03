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
module JavaQ.HR where

-- base
import           Data.Maybe

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
import           Jvmhs.Data.Named

import           Debug.Trace         as T

newtype HR = HR { getHierarchy :: HashMap.HashMap ClassName HRInfo }
  deriving (Show, Eq)

emptyHR :: HR
emptyHR = HR HashMap.empty

instance ToJSON HR where
  toJSON (HR a) = toJSON a

instance FromJSON HR where
  parseJSON v = HR <$> parseJSON v

data HRInfo = HRInfo
  { _hrExtendedBy      :: Set.HashSet ClassName
  , _hrSuperclasses    :: [ClassName]
  , _hrImplements      :: Set.HashSet ClassName
  , _hrImplementedBy   :: Set.HashSet ClassName
  , _hrIsInterface     :: Bool
  -- , _hrCallableMethods :: HashMap.HashMap MethodName (Set.HashSet ClassName)
  } deriving (Show, Eq)

makeLenses ''HRInfo

emptyHRInfo :: HRInfo
emptyHRInfo =
  HRInfo Set.empty [] Set.empty Set.empty False

addNode :: HR -> Class -> HR
addNode (HR hm) cls = HR hm'
  where
    hm' =
      HashMap.singleton (cls ^. className) chi
      <> updatedSuperclasses
      <> updatedImplementedByAbove
      <> updatedImplementsBelow
      <> hm

    chi = getOrEmpty (cls ^. className)
      & hrSuperclasses .~ superclasses
      & hrImplements <>~ clsInterfaces
      & hrIsInterface .~ isInterface cls
      -- & hrCallableMethods %~ HashMap.union fullMethods

    -- Creates list of superclasses for node class
    superclasses =
      (maybeToList $ cls ^. classSuper) ++ (fromMaybe [] $ do
        cn <- cls ^. classSuper
        view hrSuperclasses <$> HashMap.lookup cn hm)

    -- get all interfaces and then parent interfaces' interfaces
    clsInterfaces =
      Set.foldr
        (\clsnm acc -> acc <> (getOrEmpty clsnm) ^. hrImplements)
        (cls ^. classInterfaces)
        (cls ^. classInterfaces)

    foldedInterfaces = 
      chi ^. (hrImplements.folded).to getOrEmpty . hrImplements

    -- find all direct class methods
    clsMethods =
      HashMap.fromList $ do
        mn <-
          (cls ^.. classMethodList . folded .
           filtered (\m -> not $ m ^. methodAccessFlags . contains MAbstract) .
           name)
        return (mn, Set.singleton $ cls ^. className)

    extends = Set.singleton (cls ^. className) <> chi ^. hrExtendedBy
    implements = Set.singleton (cls ^. className) <> chi ^. hrImplements
    implementedBy = Set.singleton (cls ^. className) <> chi ^. hrImplementedBy

    -- Update extendedBy of all superclasses
    -- update all classes above me
    updatedSuperclasses =
      HashMap.fromList $ do
        cn <- superclasses
        return (cn, getOrEmpty cn & hrExtendedBy <>~ extends 
                                  & hrImplementedBy <>~ implementedBy)
                                  -- & hrCallableMethods %~ insertOnFind cn (chi ^. hrCallableMethods))

    insertOnFind clsnm foldedMap insertMap = 
      HashMap.foldrWithKey
        (\mthd set parentMthds -> 
          HashMap.adjust 
            (\insertSet -> if (Set.member clsnm insertSet) 
              then set <> insertSet
              else insertSet)
            mthd
            parentMthds)
        insertMap
        foldedMap

    updatedImplementedByAbove =
      HashMap.fromList $ do
        cn <- Set.toList (chi ^. hrImplements)
        return (cn, getOrEmpty cn & hrImplementedBy <>~ extends <> implementedBy)
                                  -- & hrCallableMethods %~ insertOnFind cn (chi ^. hrCallableMethods))

    -- update all classes lower than me
    updatedExtBy =
      HashMap.fromList $ do
        cn <- Set.toList (chi ^. hrExtendedBy)
        return (cn, getOrEmpty cn & hrSuperclasses %~ (superclasses ++))

    -- need to check if class implements if not found 
    -- BUG: need to update unionWith function to account when double parents of class and interface
    -- add a bool and then filter it to update those parents
    updatedImplementsBelow =
      HashMap.fromList $ do
        cn <- Set.toList (chi ^. hrExtendedBy <> chi ^. hrImplementedBy)
        return 
          ( cn
          , getOrEmptyMap cn updatedExtBy
            & hrImplements <>~ implements
            -- & hrCallableMethods %~ updateCallable cn
          )
    -- updateCallable clsnm =
    --   HashMap.unionWith
    --     (\parentSet childSet ->
    --        if (Set.member clsnm childSet)
    --          then childSet
    --          else parentSet <> childSet) $
    --   chi ^. hrCallableMethods

    getOrEmptyMap cn hmap = fromMaybe (getOrEmpty cn) $ HashMap.lookup cn hmap

    getOrEmpty cn = fromMaybe emptyHRInfo $ HashMap.lookup cn hm

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 3}''HRInfo)
