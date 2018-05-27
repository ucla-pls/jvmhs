{-|
Module      : Jvmhs.Data.Graph
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module provides an interface to a graph related library. This module
serves 2 proposes, to separate the choice of graph library from the rest of
the code, and to thoroughly document the functions used there.
-}
module Jvmhs.Data.Graph
  ( Graph
  , mkGraphFromEdges

  -- For printing
  , graphToDot
  ) where

import           Control.Lens
import           Data.Monoid                       ((<>))

import qualified Data.Set                          as S

import           Data.Graph.Inductive.Dot (showDot, fglToDot)

import           Data.Graph.Inductive.Basic
import           Data.Graph.Inductive.Graph        (mkGraph)
import           Data.Graph.Inductive.NodeMap
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.DFS

-- | The graph representation
type Graph v e = Gr v e

-- | Create a graph from a collection of edges
mkGraphFromEdges ::
  (Foldable f, Ord v, Ord e)
  => f (v,v,e)
  -> Graph v e
mkGraphFromEdges edges =
  mkGraph nodes_ edges_
  where
    nodes = edges ^.folded.(_1 <> _2).to S.singleton
    (nodes_, nodeMap) = mkNodes new (S.toList nodes)
    Just edges_ = mkEdges nodeMap (toListOf folded edges)

-- | Output the graph as a dot graph string
graphToDot ::
  (Show v, Show e)
  => Graph v e
  -> String
graphToDot = showDot . fglToDot


-- condensate :: Graph v e -> Graph [Int] ()
-- condensate = condensation
