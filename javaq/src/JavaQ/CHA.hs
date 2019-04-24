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

import           Data.Maybe

-- unordered
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as Set

-- aeson
import           Data.Aeson
import           Data.Aeson.TH

-- lens
import           Control.Lens hiding (argument, (.=))

-- jvmhs
import           Jvmhs

import           Jvmhs.Data.Named

import Debug.Trace as T

newtype CHA = CHA { getCHA :: HashMap.HashMap ClassName ClassHierarchyInfo }
  deriving (Show, Eq)

emptyCHA :: CHA
emptyCHA = CHA HashMap.empty

instance ToJSON CHA where
  toJSON (CHA a) = toJSON a

instance FromJSON CHA where
  parseJSON v = CHA <$> parseJSON v

data ClassHierarchyInfo = ClassHierarchyInfo
  { _chaExtendedBy :: Set.HashSet ClassName
  , _chaSuperclasses :: [ClassName]
  , _chaImplements :: Set.HashSet ClassName
  , _chaImplementedBy :: Set.HashSet ClassName
  , _chaIsInterface :: Bool
  , _chaCallableMethods :: HashMap.HashMap MethodName ClassName
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
      & chaCallableMethods %~ pullMethods fullMethods

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

    -- find all direct class methods
    clsMethods =
      HashMap.fromList $ do
        mn <-
          (cls ^.. classMethodList . folded .
           filtered (\m -> not $ m ^. methodAccessFlags . contains MAbstract) .
           name)
        return (mn, cls ^. className)

    superMethods =
      chi ^. (chaSuperclasses.folded).to getOrEmpty . chaCallableMethods
    itfcMethods =
      chi ^. (chaImplements.folded).to getOrEmpty . chaCallableMethods

    fullMethods = pullMethods (pullMethods clsMethods superMethods) itfcMethods

    pullMethods accMap =
      HashMap.foldrWithKey
        (\mthd clsnm mmap -> if HashMap.member mthd mmap then mmap else HashMap.insert mthd clsnm mmap)
        accMap

    -- If the i am not an interface, and the inherited method of the implementer is not by a intfc
    -- then take my version. Otherwise, do not modify (keep clsnm2)
    override =
      (\clsnm1 clsnm2 ->
         if ((getOrEmpty clsnm2) ^. chaIsInterface && not ((getOrEmpty clsnm1) ^. chaIsInterface))
           then clsnm1
           else clsnm2)

    extends = Set.singleton (cls ^. className) <> chi ^. chaExtendedBy
    implements = Set.singleton (cls ^. className) <> chi ^. chaImplements
    implementedBy = Set.singleton (cls ^. className) <> chi ^. chaImplementedBy

    -- Update extendedBy of all superclasses
    -- update all classes above me
    updatedSuperclasses =
      HashMap.fromList $ do
        cn <- superclasses
        return (cn, getOrEmpty cn & chaExtendedBy <>~ extends & chaImplementedBy <>~ implementedBy)

    updatedImplementedByAbove =
      HashMap.fromList $ do
        cn <- Set.toList (chi ^. chaImplements)
        return (cn, getOrEmpty cn & chaImplementedBy <>~ extends <> implementedBy)

    -- update all classes lower than me
    -- I think BUG is here: these two maps are not distinct
    updatedExtByAndImpl =
      HashMap.fromList $ do
        cn <- Set.toList (chi ^. chaExtendedBy)
        return (cn, getOrEmpty cn & chaSuperclasses %~ (superclasses ++))

    updatedImplementsBelow =
      HashMap.fromList $ do
        cn <- Set.toList (chi ^. chaExtendedBy <> chi ^. chaImplementedBy)
        return (cn, getOrEmptyMap cn updatedExtByAndImpl & chaImplements <>~ implements
                                  & chaCallableMethods %~ (HashMap.unionWith override $ chi ^. chaCallableMethods))

    getOrEmptyMap cn hmap = fromMaybe (getOrEmpty cn) $ HashMap.lookup cn hmap

    getOrEmpty cn = fromMaybe emptyClassHierarchyInfo $ HashMap.lookup cn hm

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 4} ''ClassHierarchyInfo)
