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
  } deriving (Show, Eq)

makeLenses ''ClassHierarchyInfo

emptyClassHierarchyInfo :: ClassHierarchyInfo
emptyClassHierarchyInfo =
  ClassHierarchyInfo Set.empty []

toClassHierarchyInfo :: Class -> ClassHierarchyInfo
toClassHierarchyInfo cls =
  ClassHierarchyInfo (Set.singleton $ cls ^. className) (maybeToList $ cls ^. classSuper)

addNode :: CHA -> Class -> CHA
addNode (CHA hm) cls = CHA hm'
  where
    hm' =
      HashMap.singleton (cls ^. className) chi 
      <> updatedSuperclasses 
      <> updatedExtendedBy 
      <> hm
    
    chi = getOrEmpty (cls ^. className) & chaSuperclasses .~ superclasses
 
    superclasses =
      (maybeToList $ cls ^. classSuper) ++ (fromMaybe [] $ do
        cn <- cls ^. classSuper
        view chaSuperclasses <$> HashMap.lookup cn hm)

    extends = Set.singleton (cls ^. className) <> chi ^. chaExtendedBy
    
    updatedSuperclasses =
      HashMap.fromList $ do
        cn <- superclasses
        return (cn, getOrEmpty cn & chaExtendedBy <>~ extends)
   
    updatedExtendedBy =
      HashMap.fromList $ do
        cn <- Set.toList (chi ^. chaExtendedBy)
        return (cn, getOrEmpty cn & chaSuperclasses %~ (superclasses ++))
  
    getOrEmpty cn = fromMaybe emptyClassHierarchyInfo $ HashMap.lookup cn hm

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 4} ''ClassHierarchyInfo)
    