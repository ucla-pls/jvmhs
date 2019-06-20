{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-
Module      : Jvmhs.Analysis.Hierarchy
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : BSD3
Maintainer  : kalhuage@cs.ucla.edu

This module defines the a class hierarchy analysis. Most of the functions in
this module expects a type checking hierarchy.
-}
module Jvmhs.Analysis.Hierarchy
  (
  -- * Hierarchy
    Hierarchy (..)
  , hryStubs
  , hryGraph

  , HierarchyStub (..)
  , hrySuper
  , hryInterfaces
  , hryFields
  , hryMethods

  , HierarchyStubs

  , toStub
  , allStubs
  , expandStubs

  -- ** Creation
  , getHierarchy
  , getHierarchyWithStubs
  , calculateHierarchy

  -- ** Classes
  , implementations

  -- ** Methods
  , definitions
  , declaration
  , callSites
  , requiredMethods
  ) where


-- lens
import           Control.Lens

-- aeson
import           Data.Aeson
import           Data.Aeson.TH

-- base
import           Control.Monad
import           Data.Functor
import           Data.Hashable
import           Data.Maybe
import qualified Data.Set
import           GHC.Generics        (Generic)

-- unordered-containers
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

-- jvmhs
import           Jvmhs.ClassPool
import           Jvmhs.Data.Class
import           Jvmhs.Data.Graph
import           Jvmhs.Data.Type

-- import qualified Data.Set.Lens       as S

-- | from map utils
fromMap' :: M.HashMap k v -> S.HashSet k
fromMap' m = S.fromMap $ m $> ()

-- | A hierarchy stub, is the only information needed to calculate the
-- hierarchy. The benifit is that this can be loaded up front.
data HierarchyStub = HierarchyStub
  { _hrySuper      :: Maybe ClassName
  , _hryInterfaces :: S.HashSet ClassName
  , _hryMethods    :: M.HashMap MethodName Bool
  , _hryFields     :: S.HashSet FieldName
  } deriving (Show, Eq)

makeLenses ''HierarchyStub
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 4} ''HierarchyStub)

toStub :: Class -> HierarchyStub
toStub cls = HierarchyStub
  (cls ^. classSuper)
  (cls ^. classInterfaces)
  (cls ^. classMethods & traverse %~ views methodAccessFlags (Data.Set.member MAbstract))
  (cls ^. classFields . to fromMap')

type HierarchyStubs = M.HashMap ClassName HierarchyStub

allStubs ::
  MonadClassPool m
  => m HierarchyStubs
allStubs =
   M.fromList <$> mapClasses (\c -> (c^.className, toStub c))

expandStubs ::
  MonadClassPool m
  => HierarchyStubs
  -> m HierarchyStubs
expandStubs old = do
  s <- allStubs
  return $ s <> old

data HEdge
  = Implement
  | Extend
  deriving (Show, Ord, Eq, Generic)

instance Hashable HEdge


data Hierarchy = Hierarchy
  { _hryStubs :: HierarchyStubs
  , _hryGraph :: Graph ClassName HEdge
  }

makeLenses ''Hierarchy

getHierarchy ::
  MonadClassPool m
  => m ([ClassName], Hierarchy)
getHierarchy =
  getHierarchyWithStubs mempty

getHierarchyWithStubs ::
  MonadClassPool m
  => HierarchyStubs
  -> m ([ClassName], Hierarchy)
getHierarchyWithStubs =
  expandStubs >=> return . calculateHierarchy

calculateHierarchy ::
  HierarchyStubs
  -> ([ClassName], Hierarchy)
calculateHierarchy stubs = do
  (missed, Hierarchy stubs (mkGraph nodes edges))
  where
    (nodes, edges) = stubs ^. ifolded.withIndex.to collection
    missed = S.toList (nodes `S.difference` fromMap' stubs)
    collection (cn, stub) =
      let toedge x = to (cn,,x) in
      ( cn `S.insert` view (hrySuper.folded. to S.singleton <> hryInterfaces) stub
      , S.fromList $ toListOf (hrySuper.folded.toedge Extend <> hryInterfaces.folded.toedge Implement) stub
      )

-- | Returns a list of all classes that implement some interface, or extends
-- a class, including the class itself.
implementations ::
  Hierarchy
  -> ClassName
  -> [ClassName]
implementations hry =
  revReachable (hry ^. hryGraph)

definitions ::
  Hierarchy
  -> AbsMethodName
  -> S.HashSet ClassName
definitions hry mid =
  S.fromList
  . toListOf (folded.filtered (methodDefinedIn hry $ mid^._2))
  . implementations hry
  $ mid^._1

declaration ::
  Hierarchy
  -> AbsMethodName
  -> Maybe AbsMethodName
declaration hry mid =
  (\a -> mid & _1.~ a) <$> go (mid ^. _1)
  where
    ii = mid ^. _2
    go cn =
      case hry ^. hryStubs.at cn of
        Nothing -> Nothing
        Just stub ->
          flip firstOf stub $
             (hryMethods.at ii._Just.like cn)
             <> (hrySuper.folded.to go._Just)
             <> (hryInterfaces.folded.to go._Just)

isAbstract :: Hierarchy -> AbsMethodName -> Maybe Bool
isAbstract hry mid =
  hry ^? hryStubs . at (mid^._1) . _Just . hryMethods . at (mid^._2) ._Just

isRequired ::
  Hierarchy
  -> AbsMethodName
  -> Bool
isRequired hry mid
  | mid ^. _2 == "<init>:()V" = True
  | otherwise =
    case hry ^. hryStubs.at (mid ^. _1) of
      Nothing -> False
      Just stub ->
        case stub ^? hrySuper._Just.to (declaration hry.flip (set _1) mid)._Just of
          Just w -> fromMaybe True (isAbstract hry w)
          Nothing ->
            orOf (hryInterfaces.folded.to go) stub
  where
    go cn =
      case hry ^. hryStubs.at cn of
        Nothing -> True
        Just stub ->
          orOf (hryMethods.at(mid^. _2)._Just <> hryInterfaces.folded.to go) stub

requiredMethods ::
  Hierarchy
  -> Class
  -> [AbsMethodName]
requiredMethods hry cls =
  cls ^.. classAbsMethodNames . filtered (isRequired hry)

methodDefinedIn ::
  Hierarchy
  -> MethodName
  -> ClassName
  -> Bool
methodDefinedIn hry mid cn =
  orOf (hryStubs.at cn._Just.hryMethods.at mid._Just.to not) hry

-- | returns a list of possible call sites.
callSites ::
  Hierarchy
  -> AbsMethodName
  -> [ AbsMethodName ]
callSites hry mid =
  definitions hry mid ^.. folded . to (\cn -> mid & _1 .~ cn)
