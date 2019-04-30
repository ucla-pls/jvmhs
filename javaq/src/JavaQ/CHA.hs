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
module JavaQ.CHA where

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

newtype CHA = CHA { getCHA :: HashMap.HashMap ClassName ClassHierarchyInfo }
  deriving (Show, Eq)

emptyCHA :: CHA
emptyCHA = CHA HashMap.empty

instance ToJSON CHA where
  toJSON (CHA a) = toJSON a

instance FromJSON CHA where
  parseJSON v = CHA <$> parseJSON v

data ClassHierarchyInfo = ClassHierarchyInfo
  { _chaExtendedBy      :: Set.HashSet ClassName
  , _chaSuperclasses    :: [ClassName]
  , _chaImplements      :: Set.HashSet ClassName
  , _chaImplementedBy   :: Set.HashSet ClassName
  , _chaIsInterface     :: Bool
  , _chaCallableMethods :: HashMap.HashMap MethodName (Set.HashSet ClassName)
  } deriving (Show, Eq)

makeLenses ''ClassHierarchyInfo

emptyClassHierarchyInfo :: ClassHierarchyInfo
emptyClassHierarchyInfo =
  ClassHierarchyInfo Set.empty [] Set.empty Set.empty False HashMap.empty

toClassHierarchyInfo :: Class -> ClassHierarchyInfo
toClassHierarchyInfo cls =
  ClassHierarchyInfo
    (Set.singleton $ cls ^. className)
    (maybeToList $ cls ^. classSuper)
    (cls ^. classInterfaces)
    Set.empty
    (isInterface cls)
    HashMap.empty

addNode :: CHA -> Class -> CHA
addNode (CHA hm) cls = CHA hm'
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

    fullMethods = HashMap.union (HashMap.union clsMethods superMethods) itfcMethods

    extends = Set.singleton (cls ^. className) <> chi ^. chaExtendedBy
    implements = Set.singleton (cls ^. className) <> chi ^. chaImplements
    implementedBy = Set.singleton (cls ^. className) <> chi ^. chaImplementedBy

    -- Update extendedBy of all superclasses
    -- update all classes above me
    updatedSuperclasses =
      HashMap.fromList $ do
        cn <- superclasses
        return (cn, getOrEmpty cn & chaExtendedBy <>~ extends 
                                  & chaImplementedBy <>~ implementedBy)
                                  -- & chaCallableMethods %~ (HashMap.unionWith 
                                  --                           (\lSet rSet -> lSet <> rSet)
                                  --                           (chi ^. chaCallableMethods)))
    insertOnFind parentMap = HashMap.foldrWithKey
        (\mthd set parentMthds -> 
          HashMap.adjust 
            (\parentSet -> if (Set.member (cls ^. className) parentSet) 
              then set <> parentSet
              else parentSet)
            mthd
            parentMthds)
        parentMap
        (chi ^. chaCallableMethods)

    updatedImplementedByAbove =
      HashMap.fromList $ do
        cn <- Set.toList (chi ^. chaImplements)
        return (cn, getOrEmpty cn & chaImplementedBy <>~ extends <> implementedBy)
                                  -- & chaCallableMethods %~ (HashMap.unionWith 
                                  --                           (\lSet rSet -> lSet <> rSet)
                                  --                           (chi ^. chaCallableMethods)))

    -- update all classes lower than me
    updatedExtBy =
      HashMap.fromList $ do
        cn <- Set.toList (chi ^. chaExtendedBy)
        return (cn, getOrEmpty cn & chaSuperclasses %~ (superclasses ++))

    -- need to check if class implements if not found 
    updatedImplementsBelow =
      HashMap.fromList $ do
        cn <- Set.toList (chi ^. chaExtendedBy <> chi ^. chaImplementedBy)
        return (cn, getOrEmptyMap cn updatedExtBy & chaImplements <>~ implements
                                  & chaCallableMethods %~ (HashMap.unionWith
                                                            (\parentSet childSet -> 
                                                              if (Set.member cn childSet)
                                                                then childSet
                                                                else parentSet <> childSet) 
                                                            $ chi ^. chaCallableMethods))

    getOrEmptyMap cn hmap = fromMaybe (getOrEmpty cn) $ HashMap.lookup cn hmap

    getOrEmpty cn = fromMaybe emptyClassHierarchyInfo $ HashMap.lookup cn hm

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 4} ''ClassHierarchyInfo)
