{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
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

  , HierarchyType (..)

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
  , declarations
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
import qualified Data.List as L
import           Data.Functor
import           Data.Hashable
import           Data.Maybe
import           GHC.Generics        (Generic)

-- unordered-containers
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

-- jvmhs
import           Jvmhs.ClassPool
import           Jvmhs.Data.Class
import           Jvmhs.Data.Graph
import           Jvmhs.Data.Type
import           Jvmhs.Data.Named

-- import qualified Data.Set.Lens       as S

-- | from map utils
fromMap' :: M.HashMap k v -> S.HashSet k
fromMap' m = S.fromMap $ m $> ()

data HierarchyType
  = HInterface
  | HAbstract
  | HPlain
  deriving (Show, Eq)

-- | A hierarchy stub, is the only information needed to calculate the
-- hierarchy. The benifit is that this can be loaded up front.
data HierarchyStub = HierarchyStub
  { _hryType       :: HierarchyType
  , _hrySuper      :: Maybe ClassName
  , _hryInterfaces :: S.HashSet ClassName
  , _hryMethods    :: M.HashMap MethodName Bool
  , _hryFields     :: S.HashSet FieldName
  } deriving (Show, Eq)

makeLenses ''HierarchyStub
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 4} ''HierarchyType)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 4} ''HierarchyStub)

toStub :: Class -> HierarchyStub
toStub cls = HierarchyStub
  ( if
      | cls ^. classAccessFlags . contains CInterface -> HInterface
      | cls ^. classAccessFlags . contains CAbstract -> HAbstract
      | otherwise -> HPlain
  )
  (cls ^? classSuper._Just.classTypeName)
  (S.fromList $ cls ^.. classInterfaces.folded.classTypeName)
  (M.fromList
   $ cls ^.. classMethods
   . folded
   . to (\m -> (m^.name, m ^. methodAccessFlags . contains MAbstract))
  )
  (S.fromList $ cls ^.. classFields . folded . name)

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
calculateHierarchy stubs =
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

-- | Return a set of classes that implements a method.
definitions ::
  Hierarchy
  -> AbsMethodName
  -> S.HashSet ClassName
definitions hry mid =
  S.fromList
  . toListOf (folded.filtered (methodDefinedIn hry $ mid^.inClassId))
  . implementations hry
  $ mid^.inClassName

-- | Returns the possible declaration of a method. It might return
-- itself if it is not abstracts
declaration ::
  Hierarchy
  -> AbsMethodName
  -> Maybe AbsMethodName
declaration hry mid =
  (\a -> mid & inClassName .~ a) <$> go (mid ^. inClassName)
  where
    ii = mid ^. relMethodName
    go cn =
      firstOf
      ( hryStubs.ix cn .
        ( hryMethods.ix ii.like cn
          <> hrySuper.folded.to go._Just
          <> hryInterfaces.folded.to go._Just
        )
      ) hry

-- | Return a list of abstract methods that declares the method, but not the
-- method itself. If a method is defined above this, no declaration above this 
-- is used.
declarations :: Hierarchy -> AbsMethodName -> [AbsMethodName]
declarations hry m =
  hry ^.. hryStubs . ix (m ^.inClassName) . folding abstractsInSupers
  where
    abstractsInSupers = toListOf $
      (hrySuper._Just <> hryInterfaces.folded)
      . folding abstractsInClass

    abstractsInClass cn =
      case hry ^. hryStubs.at cn of
        Just stb ->
          case stb ^. hryMethods.at mid of
            Just True -> [mkAbsMethodName cn mid]
            _ | stb ^. hryType `L.elem` [HInterface, HAbstract]
                -> abstractsInSupers stb
              | otherwise
                -> []
        Nothing -> []

    mid = m ^. relMethodName


-- isAbstract :: Hierarchy -> AbsMethodName -> Maybe Bool
-- isAbstract hry mid =
--   hry ^? hryStubs . ix (mid^.inClassName) . hryMethods . ix (mid^.inClassId)

-- higherMethods :: Hierarchy -> MethodName -> Fold ClassName AbsMethodName
-- higherMethods hry =

-- | A fold of all abstract super classes, this includes classes and
-- intefaces
abstractedSuperClasses :: HierarchyStub -> Hierarchy -> [HierarchyStub]
abstractedSuperClasses stub h =
  toListOf (
    (hrySuper._Just <> hryInterfaces.folded)
    . folding (\cn -> h ^? hryStubs.ix cn)
    . filtered (\n -> n ^. hryType `L.elem` [HInterface, HAbstract])
    ) stub



isRequired ::
  Hierarchy
  -> AbsMethodName
  -> Bool
isRequired hry m =
  maybe False isAbstractInSupers $ hry ^. hryStubs.at (m ^.inClassName)
  where
    isAbstractInSupers :: HierarchyStub -> Bool
    isAbstractInSupers stb =
      or (map hasAbstract $ abstractedSuperClasses stb hry)

    hasAbstract :: HierarchyStub -> Bool
    hasAbstract stb =
      fromMaybe (isAbstractInSupers stb) $ stb ^. hryMethods.at mid

    mid = m ^. relMethodName

  -- where go


--   | mid ^. relMethodName.methodId == "<init>:()V" = True
--   | otherwise =
--     orOf
--     (
--       hryStubs
--       . ix (mid ^. inClassName)
--       . ( hrySuper._Just.to (isAbstractIn (mid ^. relMethodName))
--         )
--     )

--     case hry ^.
--       Nothing -> False
--       Just stub ->
--         orOf
--         ( hrySuper._Just.to (declaration hry.flip (set inClassName) mid)._Just
--         )
--         case stub ^?  of
--           Just w -> fromMaybe True (isAbstract hry w)
--           Nothing ->
--             orOf (hryInterfaces.folded.to go) stub
--   where
--     go cn =
--       case hry ^. hryStubs.at cn of
--         Nothing -> True
--         Just stub ->
--           orOf ( hryMethods . ix (mid^. relMethodName)
--                  <> hryInterfaces.folded.to go
--                ) stub



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
  definitions hry mid ^.. folded . to (\cn -> mid & inClassName .~ cn)
