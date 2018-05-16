{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-
Module      : Jvmhs.Analysis.Hierarchy
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module defines the a class hierarchy analysis.
-}
module Jvmhs.Analysis.Hierarchy
  (
  -- * Hierachy

  -- ** Creation
    Hierarchy(..)
  , hryNodes
  , hryImplements
  , hryExtends
  , calculateHierarchy

  -- ** Accesses
  , subclasses
  , superclasses
  ) where


import           Control.Lens

import           Data.Monoid
-- import           Data.Set
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.NodeMap
import           Data.Graph.Inductive.Query.DFS

import           Jvmhs.ClassPool
import           Jvmhs.Data.Type
import           Jvmhs.Data.Class

-- | The class hierarchy analysis results in two graphs, a implements graph and
-- an extends graph.
data Hierarchy = Hierarchy
  { _hryNodes      :: NodeMap ClassName
  , _hryExtends    :: Gr ClassName ()
  , _hryImplements :: Gr ClassName ()
  } deriving (Show, Eq)

makeLenses ''Hierarchy

-- | Given a foldable structure over 'ClassName's compute a Hierarchy.
calculateHierarchy ::
  (Foldable t, MonadClassPool m)
  => t ClassName
  -> m Hierarchy
calculateHierarchy clss = do
  (classNames, supers, interfaces) <- clss ^! folded . load . to collection
  let
    (nodes_, nodeMap) = mkNodes new classNames
    Just superEdges = mkEdges nodeMap supers
    Just interfaceEdges = mkEdges nodeMap interfaces
  return $
    Hierarchy
      nodeMap
      (mkGraph nodes_ superEdges)
      (mkGraph nodes_ interfaceEdges)
  where
    collection cls =
      ( toListOf (className <> classSuper <> classInterfaces.folded) cls
      , cls^..classSuper.to (cls^.className,,())
      , cls^..classInterfaces.folded.to (cls^.className,,())
      )

-- | Given a Hierarchy, determine the subclasses of a class.
subclasses ::
     Hierarchy
  -> ClassName
  -> [ClassName]
subclasses hry cln =
  tail $ rdfs [nodeOf cln] graph ^.. folded . to (lab graph) . _Just
  where
    graph = (hry^.hryExtends)
    nodeOf = fst . mkNode_ (hry^.hryNodes)

-- | Given a Hierarchy, determine the superclasses of a class.
-- The returned list is in order as they appear in the class hierarchy.
superclasses ::
     Hierarchy
  -> ClassName
  -> [ClassName]
superclasses hry cln =
  tail $ dfs [nodeOf cln] graph ^.. folded . to (lab graph) . _Just
  where
    graph = (hry^.hryExtends)
    nodeOf = fst . mkNode_ (hry^.hryNodes)
