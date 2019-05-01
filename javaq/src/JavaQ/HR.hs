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
  { _chaExtendedBy      :: Set.HashSet ClassName
  , _chaSuperclasses    :: [ClassName]
  , _chaImplements      :: Set.HashSet ClassName
  , _chaImplementedBy   :: Set.HashSet ClassName
  , _chaIsInterface     :: Bool
  , _chaCallableMethods :: HashMap.HashMap MethodName (Set.HashSet ClassName)
  } deriving (Show, Eq)

makeLenses ''HRInfo

emptyHRInfo :: HRInfo
emptyHRInfo =
  HRInfo Set.empty [] Set.empty Set.empty False HashMap.empty

-- buildMethods :: HR -> CHA
-- buildMethods (HR hm) = CHA hm
--   where
--     ord = getClassOrder hm


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
      & chaSuperclasses .~ superclasses
      & chaImplements <>~ clsInterfaces
      & chaIsInterface .~ isInterface cls
      & chaCallableMethods %~ HashMap.union fullMethods

    -- Creates list of superclasses for node class
    superclasses =
      (maybeToList $ cls ^. classSuper) ++ (fromMaybe [] $ do
        cn <- cls ^. classSuper
        view chaSuperclasses <$> HashMap.lookup cn hm)

    -- get all interfaces and then parent interfaces' interfaces
    clsInterfaces =
      Set.foldr
        (\clsnm acc -> acc <> (getOrEmpty clsnm) ^. chaImplements)
        (cls ^. classInterfaces)
        (cls ^. classInterfaces)

    foldedInterfaces = 
      chi ^. (chaImplements.folded).to getOrEmpty . chaImplements

    -- find all direct class methods
    clsMethods =
      HashMap.fromList $ do
        mn <-
          (cls ^.. classMethodList . folded .
           filtered (\m -> not $ m ^. methodAccessFlags . contains MAbstract) .
           name)
        return (mn, Set.singleton $ cls ^. className)

    -- NOTE: need to check if it unions or not
    superMethods =
      chi ^. (chaSuperclasses.folded).to getOrEmpty . chaCallableMethods
    itfcMethods =
      chi ^. (chaImplements.folded).to getOrEmpty . chaCallableMethods
    upperMethods = HashMap.union (HashMap.union clsMethods superMethods) itfcMethods
    lowerMethodsImpl = 
      chi ^. (chaImplementedBy.folded).to getOrEmpty . chaCallableMethods 
    
    -- fullMethods = HashMap.empty

    fullMethods = 
      insertOnFind (cls ^. className) lowerMethodsImpl upperMethods

    extends = Set.singleton (cls ^. className) <> chi ^. chaExtendedBy
    implements = Set.singleton (cls ^. className) <> chi ^. chaImplements
    implementedBy = Set.singleton (cls ^. className) <> chi ^. chaImplementedBy

    -- Update extendedBy of all superclasses
    -- update all classes above me
    updatedSuperclasses =
      HashMap.fromList $ do
        cn <- superclasses
        return (cn, getOrEmpty cn & chaExtendedBy <>~ extends 
                                  & chaImplementedBy <>~ implementedBy
                                  & chaCallableMethods %~ insertOnFind cn (chi ^. chaCallableMethods))

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
        cn <- Set.toList (chi ^. chaImplements)
        return (cn, getOrEmpty cn & chaImplementedBy <>~ extends <> implementedBy
                                  & chaCallableMethods %~ insertOnFind cn (chi ^. chaCallableMethods))

    -- update all classes lower than me
    updatedExtBy =
      HashMap.fromList $ do
        cn <- Set.toList (chi ^. chaExtendedBy)
        return (cn, getOrEmpty cn & chaSuperclasses %~ (superclasses ++))

    -- need to check if class implements if not found 
    -- BUG: need to update unionWith function to account when double parents of class and interface
    -- add a bool and then filter it to update those parents
    updatedImplementsBelow =
      HashMap.fromList $ do
        cn <- Set.toList (chi ^. chaExtendedBy <> chi ^. chaImplementedBy)
        return 
          ( cn
          , getOrEmptyMap cn updatedExtBy
            & chaImplements <>~ implements
            & chaCallableMethods %~ updateCallable cn
          )
    updateCallable clsnm =
      HashMap.unionWith
        (\parentSet childSet ->
           if (Set.member clsnm childSet)
             then childSet
             else parentSet <> childSet) $
      chi ^. chaCallableMethods

    getOrEmptyMap cn hmap = fromMaybe (getOrEmpty cn) $ HashMap.lookup cn hmap

    getOrEmpty cn = fromMaybe emptyHRInfo $ HashMap.lookup cn hm

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 4} ''HRInfo)
