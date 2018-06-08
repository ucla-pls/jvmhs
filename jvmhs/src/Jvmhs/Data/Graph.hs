{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts       #-}
{-|
Module      : Jvmhs.Data.Graph
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This module provides an interface to a graph related library. This module
serves 2 proposes, to separate the choice of graph library from the rest of
the code, and to thoroughly document the functions used there.
-}
module Jvmhs.Data.Graph
  ( Graph
  , mkGraph
  , mkGraphFromEdges

  -- Graph manipulations
  , sccGraph

  -- For printing
  , graphToDot

  -- * Algorithms
  , gdd
  , partition
  , binarySearch
  ) where

import           Control.Lens
import qualified Data.Map                          as M
import           Data.Maybe
import           Data.Monoid                       ((<>))
import           Data.Tuple                        (swap)

import qualified Data.IntSet as IS
import qualified Data.Vector as V
import qualified Data.List as L

import           Data.Graph.Inductive.Dot          (fglToDot, showDot)

--import           Data.Graph.Inductive.Basic
import qualified Data.Graph.Inductive.Graph        as F
-- import           Data.Graph.Inductive.NodeMap
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.DFS

import           Jvmhs.LensHelpers

-- | The graph representation
type Graph v e = Gr v e
type NodeMap v = M.Map v Int

grNodes :: IndexedFold Int (Graph v e) v
grNodes = ifolding F.labNodes

toLabel :: Graph v e -> Getter Int (Maybe v)
toLabel gr = to (F.lab gr)

-- | Create a graph from nodes and edges. Any edge where both nodes are not in
-- the inputs will be removed. *This functions assumes that there is only one
-- node of each name.*
mkGraph ::
  forall f t v e. (Foldable f, Foldable t, Ord v, Ord e)
  => f v
  -> t (v,v,e)
  -> Graph v e
mkGraph nodes edges =
  F.mkGraph nodes_ edges_
  where
    nodes_ = itoListOf folded nodes
    nodemap = M.fromList $ swap <$> nodes_
    edges_ = edges ^..folded.to nLookup._Just
    nLookup (v1,v2,e)= (,,) <$> (nodemap M.!? v1) <*> (nodemap M.!? v2) <*> pure e

-- | Create a graph from a collection of edges.
mkGraphFromEdges ::
  (Foldable f, Ord v, Ord e)
  => f (v,v,e)
  -> Graph v e
mkGraphFromEdges edges =
  mkGraph (edges^.folded.(_1 <> _2).toSet) edges

-- | Output the graph as a dot graph string
graphToDot ::
  (Show v, Show e)
  => Graph v e
  -> String
graphToDot =
  showDot . fglToDot

-- | Create a graph of strongly connected components.
sccGraph ::
  (Ord v)
  => Graph v e
  -> (NodeMap v, Graph [v] ())
sccGraph gr =
  (nodemap, gr')
  where
    condense = condensation gr
    gr' = F.nmap (fromJust . mapM (F.lab gr)) condense
    nodemap = M.fromList $ gr' ^..grNodes. withIndex . to unfoldN.traverse
    unfoldN (n,l) = map (,n) l



-- | Graph delta-debugging
gdd ::
  (Monad m)
  => Graph v e
  -- ^ A graph
  -> ([v] -> m Bool)
  -- ^ The property
  -> m [v]
  -- ^ Returns a minimal set of nodes, where the property is true
gdd gr p = do
  -- First compute the strongly connected components graph
  (_, c) <- binarySearch bs (\(s, _) -> p s)
  return $ c
  where
    bs = V.fromList
      . tail
      . L.scanl' (\(a,_) (s,c) -> (s ++ a, c)) ([], undefined)
      . partition $ gr

-- computeAccumulateClosure ::
-- computeAccumulateClosure

-- | Compute the different possible closures for a graph, returns a list of
-- unique sets and closures, with indices into the original graph.
partition :: Graph v e -> [ ([v], [v]) ]
partition gr =
  L.map (each %~ asLabels) . L.sortOn (IS.size . snd) . partition' $ gr
  where
    asLabels =
      toListOf (to (IS.toList) . folded . toLabel gr. _Just)


partition' :: Graph v e -> [ (IS.IntSet, IS.IntSet) ]
partition' gr =
  catMaybes . V.toList $ cv
  where
    sccgr = condensation gr

    cv = V.fromList [ getNode n | n <- enumFromTo 0 (snd $ F.nodeRange sccgr)]

    getNode :: Int -> Maybe (IS.IntSet, IS.IntSet)
    getNode n = do
      (_, _, s, t) <- fst $ n `F.match` sccgr
      let
        Just before = sequence [ (cv V.! b) ^? _Just._2 | (_, b) <- t ]
        s'  = IS.fromList s
        closure = IS.unions (s':before)
      return (s', closure)



-- | Binary search under the following conditions,
-- 1) the predicate has to be true at least one element
-- 2) the predicate has to be monotonic over the list
binarySearch ::
  (Monad m)
  => V.Vector e
  -> (e -> m Bool)
  -> m e
binarySearch vec test =
  go 0 (V.length vec)
  where
    go i j
      | i == j  =
        return (vec V.! i)
      | otherwise = do
        let pivot = i + ((j - i) `quot` 2)
        inLowerHalf <- test (vec V.! pivot)
        if inLowerHalf
          then go i pivot
          else go (pivot + 1) j
