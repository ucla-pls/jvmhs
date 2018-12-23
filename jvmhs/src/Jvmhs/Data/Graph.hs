{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
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
  , graphToDot'

  -- * Lenses
  , toLabel
  , grNodes
  , grNode

  , fromLabel
  , labels

  -- * Modifications
  , remove
  , forwardRemove
  , shrink

  -- * Algorithms
  , isClosedIn
  , partition
  , partition'
  , closures
  , close
  , collapse
  , revclose

  , revReachable

  , reverseFold

  -- * Re-exports
  , F.order
  , F.size

  -- * Helper functions
  , graphFromFile
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Data.Foldable                     (toList)
import qualified Data.Map                          as M
import qualified Data.Set                          as S
import           Data.Maybe
import           Data.Monoid                       ((<>))
import           Data.Tuple                        (swap)

import qualified Data.IntMap                       as IM
import qualified Data.IntSet                       as IS
import qualified Data.List                         as L
import qualified Data.Set.Lens                     as S
import qualified Data.Vector                       as V

import qualified Data.ByteString                   as BS
import           System.IO

import qualified Data.Attoparsec.ByteString.Char8  as P

import           Data.Graph.Inductive.Dot          (fglToDot, fglToDotGeneric,
                                                    showDot)

import qualified Data.Graph.Inductive.Graph        as F
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.DFS

import           Jvmhs.LensHelpers

-- | The graph representation
data Graph v e = Graph
  { _innerGraph :: !(Gr v e)
  , _nodeMap    :: !(NodeMap v)
  }

instance (NFData v, NFData e) => NFData (Graph v e) where
  rnf (Graph a b) = seq (seq (rnf a) rnf b) ()

type NodeMap v = M.Map v Int

makeLenses ''Graph

grNodes :: IndexedFold Int (Graph v e) v
grNodes = innerGraph . ifolding F.labNodes

toLabel :: Graph v e -> Getter Int (Maybe v)
toLabel gr = to (F.lab $ view innerGraph gr)

grNode :: Int -> Getter (Graph v e) (Maybe v)
grNode i = innerGraph . to (flip F.lab i)

fromLabel :: Ord v => Graph v e -> Getter v (Maybe Int)
fromLabel gr = to (`M.lookup` view nodeMap gr)

labels :: [Int] -> Graph v e -> [v]
labels vs grp = vs ^.. folded . toLabel grp . _Just

-- | Create a graph from nodes and edges. Any edge where both nodes are not in
-- the inputs will be removed. *This functions assumes that there is only one
-- node of each name.*
mkGraph ::
  forall f t v e. (Foldable f, Foldable t, Ord v, Ord e)
  => f v
  -> t (v,v,e)
  -> Graph v e
mkGraph nodes edges =
  Graph (F.mkGraph nodes_ edges_) nodemap
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

-- | Check if a foldable collection of items is closed in the graph
isClosedIn ::
  (Foldable f, Ord v)
  => f v
  -> Graph v e
  -> Bool
isClosedIn vs gr =
  closure == input
  where
    input = (L.sort $ vs ^.. folded.fromLabel gr._Just)
    closure = L.sort $ dfs input (gr^.innerGraph)

-- | Given a list of items close under them.
close ::
  (Foldable f, Ord v)
  => Graph v e
  -> f v
  -> [v]
close gr vs =
  closure ^.. folded.toLabel gr._Just
  where
    input = vs ^.. folded.fromLabel gr._Just
    closure = dfs input (gr^.innerGraph)

-- | Given a list of items, remove all items that is not closed.
collapse ::
  (Foldable f, Ord v)
  => Graph v e
  -> f v
  -> [v]
collapse gr vs =
  left ^.. to IS.toList.folded.toLabel gr._Just
  where
    input = vs ^.. folded.fromLabel gr._Just
    inputSet = IS.fromList input
    missed =
      IS.fromList (dfs input (gr^.innerGraph))
      `IS.difference` inputSet
    left =
      inputSet
      `IS.difference`
      IS.fromList (rdfs (IS.toList missed) (gr^.innerGraph))

revclose ::
  (Ord v)
  => Graph v e
  -> [v]
  -> [v]
revclose gr vs =
  closure ^.. folded.toLabel gr._Just
  where
    input = vs ^.. folded.fromLabel gr._Just
    closure = rdfs input (gr^.innerGraph)


-- | Output the graph as a dot graph string
graphToDot ::
  (Show v, Show e)
  => Graph v e
  -> String
graphToDot =
  showDot . fglToDot . _innerGraph

-- | Output the graph as a dot graph string, with string methods
graphToDot' ::
  Graph v e
  -> (v -> String)
  -> (e -> String)
  -> String
graphToDot' gr fv fe =
  showDot $ fglToDotGeneric (_innerGraph gr) fv fe id

-- | Create a graph of strongly connected components.
sccGraph ::
  (Ord v)
  => Graph v e
  -> (NodeMap v, Graph [v] ())
sccGraph gr =
  (nodemap', Graph gr' nodemap)
  where
    condense = condensation (gr ^. innerGraph)
    gr' = F.nmap (fromJust . mapM (F.lab (gr ^. innerGraph))) condense
    nodes = gr' ^..ifolding (F.labNodes). withIndex
    nodemap = M.fromList $ nodes ^.. traverse . to swap
    nodemap' = M.fromList $ nodes ^.. traverse . to unfoldN .traverse
    unfoldN (n,l) = map (,n) l


revReachable ::
  Ord v
  => Graph v e
  -> v
  -> [v]
revReachable gr@(Graph g _) v =
  case v ^. fromLabel gr of
    Just x  -> rdfs [x] g ^.. folded . toLabel gr . _Just
    Nothing -> []

reverseFold ::
  Ord v
  => Graph v e
  -> (v -> [(v, e, m)] -> m)
  -> v
  -> Maybe m
reverseFold gr@(Graph g _) f =
  preview (fromLabel gr . _Just . to go)
  where
    go vid =
      f (gr ^?! grNode vid._Just) [ (gr^?!grNode vid'._Just, e, go vid') | (vid', e) <- F.lpre g vid ]



-- | Compute the different possible closures for a graph, returns a list of
-- unique sets and closures, with indices into the original graph.
partition :: Graph v e -> [ ([v], [v]) ]
partition gr =
  L.map (each %~ asLabels) . L.sortOn (IS.size . snd) . partition' $ gr
  where
    asLabels =
      toListOf (to (IS.toList) . folded . toLabel gr. _Just)

closures :: Graph v e -> [ IS.IntSet ]
closures = map snd . partition'

-- | The partition of the graph into closures, the first is
-- each strongly connected component and the second is the closure of
-- that component.
partition' :: Graph v e -> [ (IS.IntSet, IS.IntSet) ]
partition' graph =
  catMaybes . V.toList $ cv
  where
    gr = (graph ^. innerGraph)
    sccs = scc gr
    iscc = zip [0..] sccs
    vMap = IM.fromList . concatMap (\(i,xs) -> map (,i) xs) $ iscc

    edges = map (
      \(i, ls) ->
        IS.unions . map (IS.delete i . getEdges) $ ls
      ) iscc

    cv = V.fromList (zipWith getNode sccs edges)

    getEdges =
      IS.fromList
      . map ((vMap IM.!) . view _2)
      . F.out gr

    getNode :: [Int] -> IS.IntSet -> Maybe (IS.IntSet, IS.IntSet)
    getNode s t = do
      let
        Just before = sequence [ (cv V.! b) ^? _Just._2 | b <- IS.toList t ]
        s'  = IS.fromList s
        closure = IS.unions (s':before)
      return (s', closure)

-- | Remove elements in the graph and all nodes that they point to. Return
-- the new graph and the removed elements.
forwardRemove ::
  (Foldable f, Ord v)
  => Graph v e
  -> f v
  -> ([v], Graph v e)
forwardRemove gr f =
  (closure, remove gr closure)
  where
    closure = close gr (toList f)

remove ::
  (Foldable f, Ord v)
  => Graph v e
  -> f v
  -> Graph v e
remove gr vs =
  gr & innerGraph %~ F.delNodes (vs ^..folded.fromLabel gr._Just)
     & nodeMap %~ flip M.withoutKeys (S.setOf folded vs)

shrink ::
  (Foldable f, Ord v)
  => Graph v e
  -> f v
  -> Graph v e
shrink gr vs =
  remove gr revset
  where
    revset = S.setOf grNodes gr `S.difference` S.setOf folded vs

-- | Reads a graph from file. Expects the file to a list of two integres.
graphFromFile ::
  FilePath ->
  IO (Graph Int ())
graphFromFile filepath =
  withFile filepath ReadMode $ \h -> do
    Just g <- P.maybeResult <$> P.parseWith (BS.hGet h 4096) fp BS.empty
    return $ mkGraphFromEdges g
  where
    fp :: P.Parser [(Int, Int, ())]
    fp = P.many' lp <* P.endOfInput
    lp = (,,()) <$> P.decimal <* P.space <*> P.decimal <* P.endOfLine
