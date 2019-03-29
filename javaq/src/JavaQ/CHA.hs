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

class Closable m where
  closure :: m -> m

-- instance Closable CHA where 
--   close = closureCHA

newtype CHA = CHA { getCHA :: HashMap.HashMap ClassName ClassHierarchyInfo }
  deriving (Show, Eq)

-- instance Semigroup a => Semigroup (CHTree ClassHierarchyInfo a) where
--   CHTree ClassHierarchyInfo a <> CHTree ClassHierarchyInfo b = CHTree (HashMap.unionWith (<>) a b)

instance Semigroup CHA where
  CHA a <> CHA b = closure $ CHA (HashMap.unionWith (<>) a b)

instance Monoid CHA where
  mempty = CHA HashMap.empty

instance ToJSON CHA where
  toJSON (CHA a) = toJSON a

instance FromJSON CHA where
  parseJSON v = CHA <$> parseJSON v

instance Closable CHA where
  closure = closureCHA

data ClassHierarchyInfo = ClassHierarchyInfo
  { chaExtendedBy :: Set.HashSet ClassName
  } deriving (Show, Eq)

instance Semigroup ClassHierarchyInfo where
  ClassHierarchyInfo a <> ClassHierarchyInfo b = ClassHierarchyInfo (a <> b)

instance Monoid ClassHierarchyInfo where
  mempty = ClassHierarchyInfo mempty

classToCHA :: Class -> CHA
classToCHA cls =
  case super of
    Nothing -> CHA orig_map
    Just sup -> CHA $ HashMap.insert sup (ClassHierarchyInfo $ Set.singleton clsnm) orig_map
  where
    clsnm = cls ^. className
    -- interfs = maybeToList (cls ^. classSuper) ++ toListOf (classInterfaces.folded) cls
    inners = fmap (view innerClass) $ cls ^. classInnerClasses
    interfs = inners ++ toListOf (classInterfaces . folded) cls
    entries = map (, ClassHierarchyInfo (Set.singleton clsnm)) interfs
    orig_map =
      HashMap.fromList
        ((clsnm, ClassHierarchyInfo (Set.singleton clsnm)) : entries)
    super = cls ^. classSuper


closureCHA :: CHA -> CHA
closureCHA cha =
  let
    tmap = getCHA cha
    keys = HashMap.keys tmap
    newCHA = CHA $ mconcat $ closeClass tmap <$> keys
  in
    -- cha
    if cha == newCHA
      then newCHA
      else closureCHA newCHA


closeClass :: HashMap.HashMap ClassName ClassHierarchyInfo -> 
  ClassName -> HashMap.HashMap ClassName ClassHierarchyInfo
closeClass hmap clsnm =
  HashMap.fromList [(clsnm, implSet)]
  where
    origSet = fromJust (HashMap.lookup clsnm hmap)
    -- flipper = flip HashMap.lookup hmap clsnm
    toMerge = catMaybes $ (flip HashMap.lookup hmap) <$> (Set.toList (chaExtendedBy origSet))
    implSet = mconcat toMerge 


$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 3} ''ClassHierarchyInfo)
