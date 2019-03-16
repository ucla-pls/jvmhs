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

newtype ImplMap a = ImplMap { getImplMap :: HashMap.HashMap ClassName a }
  deriving (Show, Eq)

type CHA = ImplMap ClassHierachyInfo

data ClassHierachyInfo = ClassHierachyInfo
  { chaImplements :: Set.HashSet ClassName
  } deriving (Show, Eq)

instance Semigroup ClassHierachyInfo where
  ClassHierachyInfo a <> ClassHierachyInfo b = ClassHierachyInfo (a <> b)

instance Monoid ClassHierachyInfo where
  mempty = ClassHierachyInfo mempty

classToCHA :: Class -> CHA
classToCHA cls =
  ImplMap $ HashMap.fromList ((clsnm, ClassHierachyInfo (Set.singleton clsnm)) : entries)
  where
    clsnm = cls ^. className
    interfs = toListOf (classInterfaces.folded) cls
    entries = map (, ClassHierachyInfo (Set.singleton clsnm)) interfs

instance Semigroup a => Semigroup (ImplMap a) where
  ImplMap a <> ImplMap b = ImplMap (HashMap.unionWith (<>) a b)

instance Semigroup a => Monoid (ImplMap a) where
  mempty = ImplMap HashMap.empty

instance ToJSON a => ToJSON (ImplMap a) where
  toJSON (ImplMap a) = toJSON a

instance FromJSON a => FromJSON (ImplMap a)where
  parseJSON v = ImplMap <$> parseJSON v

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 3} ''ClassHierachyInfo)
