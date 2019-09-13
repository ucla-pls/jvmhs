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
module JavaQ.Command.ClassHierarchyAnalysis where

-- unordered
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet        as Set

-- mtl
import Control.Monad.Reader

-- aeson
import           Data.Aeson
import           Data.Aeson.TH

-- lens
import           Control.Lens        hiding (argument, (.=))

-- jvmhs
import           Jvmhs

import           JavaQ.Command.Hierarchy
import           JavaQ.Command

newtype CHA = CHA { getClassHierarchyAnalysis :: HashMap.HashMap ClassName CHAInfo }
  deriving (Show, Eq)

emptyCHA :: CHA
emptyCHA = CHA HashMap.empty

instance ToJSON CHA where
  toJSON (CHA a) = toJSON a

instance FromJSON CHA where
  parseJSON v = CHA <$> parseJSON v

data CHAInfo = CHAInfo
  { _chaExtendedBy      :: Set.HashSet ClassName
  , _chaSuperclasses    :: [ClassName]
  , _chaImplements      :: Set.HashSet ClassName
  , _chaImplementedBy   :: Set.HashSet ClassName
  , _chaIsInterface     :: Bool
  , _chaCallableMethods :: HashMap.HashMap MethodName (Set.HashSet ClassName)
  } deriving (Show, Eq)

makeLenses ''CHAInfo

emptyCHAInfo :: CHAInfo
emptyCHAInfo =
  CHAInfo Set.empty [] Set.empty Set.empty False HashMap.empty

-- topological sort on classes
getClassOrder :: HR -> [ClassName]
getClassOrder (HR _) = []

-- buildMethods :: HR -> CHA
-- buildMethods (HR hm) = CHA hm
--   where
--     ord = getClassOrder hm

    -- NOTE: need to check if it unions or not
    -- superMethods =
    --   chi ^. (hrSuperclasses.folded).to getOrEmpty . hrCallableMethods
    -- itfcMethods =
    --   chi ^. (hrImplements.folded).to getOrEmpty . hrCallableMethods
    -- upperMethods = HashMap.union (HashMap.union clsMethods superMethods) itfcMethods
    -- lowerMethodsImpl = 
    --   chi ^. (hrImplementedBy.folded).to getOrEmpty . hrCallableMethods 
    -- fullMethods = 
    --   insertOnFind (cls ^. className) lowerMethodsImpl upperMethods

        -- return (cn, getOrEmpty cn & hrExtendedBy <>~ extends 
        --                           & hrImplementedBy <>~ implementedBy
        --                           & hrCallableMethods %~ insertOnFind cn (chi ^. hrCallableMethods))

    -- updateCallable clsnm =
    --   HashMap.unionWith
    --     (\parentSet childSet ->
    --        if (Set.member clsnm childSet)
    --          then childSet
    --          else parentSet <> childSet) $
    --   chi ^. hrCallableMethods

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 4} ''CHAInfo)


chaCmd :: CommandSpec
chaCmd = CommandSpec
  "cha"
  "Cha ..."
  [ Json id ]
  (Algorithm computeCha)

computeCha :: (HasCommandConfig c, MonadIO m, MonadReader c m, MonadClassPool m) => m String
computeCha = do
  mc <- getClass "java/lang/Object"
  mf <- view cfgHierarchy
  return (show mf ++ maybe "no" (const "yes") mc)
