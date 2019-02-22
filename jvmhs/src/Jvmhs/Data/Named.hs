{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric  #-}
{-|
Module      : Jvmhs.Data.Named
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

A named container of items. This can be used to make efficient NameSets and
NameMaps.
-}

module Jvmhs.Data.Named where

-- base
import           GHC.Generics (Generic)

-- deep-seq
import           Control.DeepSeq

-- lens
import           Control.Lens

-- aeson
import Data.Aeson as A

-- unordered-containers
import qualified Data.HashMap.Strict as Map
--import qualified Data.HashSet as Set

-- hashable
import Data.Hashable

data Name n = Name { nameHash :: !Int, innerName :: !n }
  deriving (Show, Eq, Ord, NFData, Generic)

mkName :: Hashable n => n -> Name n
mkName n =
  Name (hash n) n

asName :: Hashable n => Iso' n (Name n)
asName = iso mkName innerName

instance Hashable (Name n) where
  hashWithSalt i (Name h _) =
    i `hashWithSalt` h


class HasName name n | n -> name where
  name :: Lens' n name

data Named name content =
  Named
  { namedName :: !name
  , namedContent :: !content
  }
  deriving (Show, Eq, Generic, NFData)

instance HasName e n => HasName e (Named n c) where
  name = (lens namedName (\n h -> n { namedName = h })) . name

content :: Lens' (Named name content) content
content = lens namedContent (\n h -> n { namedContent = h })

instance (ToJSON name) => ToJSON (Name name) where
  toJSON = toJSON . innerName

instance (ToJSON name, ToJSON content) => ToJSON (Named name content) where
  toJSON (Named n c) = do
    case toJSON c of
      Object m ->
        Object (Map.insert "name" (toJSON n) m)
      a ->
        object ["name" A..= n, "content" A..= a]


-- | Not a true iso morphism.
namedMapAsList :: (Hashable a, Eq a) => Iso' (Map.HashMap a b) [Named a b]
namedMapAsList =
  iso namedMapToList namedMapFromList
  where
    namedMapFromList = Map.fromList . map (\(Named n c) -> (n,c))

    namedMapToList = map (uncurry Named) . Map.toList

-- instance (ToJSON name, ToJSON content) => ToJSON (Named name content) where
--   toJSON (Named n c) = do
--     case toJSON c of
--       Object m ->
--         Object (insert "name" (toJSON n) m)
--       a ->
--         object ["name" A..= n, "content" A..= a]
