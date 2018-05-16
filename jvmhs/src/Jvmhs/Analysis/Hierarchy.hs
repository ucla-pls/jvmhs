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
  ( Hierarchy(..)
  , hryNodes
  , hryImplements
  , hryExtends
  , calculateHierarchy
  ) where


import           Control.Lens

import           Data.Monoid
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.NodeMap

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

-- | Given a foldable structure over classnames compute a Hierarchy
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
