{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts       #-}
{-|
Module      : Jvmhs.Analysis.DeltaDebug
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

This library contains delta-debugging related functions.
-}
module Jvmhs.Analysis.DeltaDebug
  ( ddmin
  , gdd
  , sdd
  , binarySearch
  ) where

import           Control.Lens
import           Control.Monad

import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.List as L

import Jvmhs.Data.Graph

-- | Graph delta-debugging
gdd ::
  (Monad m)
  => ([v] -> m Bool)
  -- ^ The property
  -> Graph v e
  -- ^ A graph
  -> m [v]
  -- ^ Returns a minimal set of nodes, where the property is true
gdd p gr = do
  -- First compute the strongly connected components graph
  let par = V.fromList . L.sortOn IS.size . map snd $ partition' gr
  fmap asLabels . sdd (p.asLabels) $ par
  where
    asLabels = toListOf (folded.toLabel gr._Just) . IS.toList

-- | Set delta-debugging
-- Given a list of sets sorted after size, then return the smallest possible
-- set such that uphold the predicate.
sdd ::
  (Monad m)
  => (IS.IntSet -> m Bool)
  -> V.Vector IS.IntSet
  -> m IS.IntSet
sdd p v = do
  let mvunion = V.scanl' IS.union IS.empty v
  i <- binarySearch mvunion p
  if i == 0
    then return IS.empty
    else do
      let s = v V.! (i - 1)
      t <- p s
      if t
        then return s
        else do
          x <- sdd p . V.map (IS.union s) $ V.take (i - 1) v
          y <- sdd p (V.fromList
                      . (++ [x])
                      . V.toList
                      . V.ifilter (\j s' -> j /= i - 1 && IS.size s' < IS.size x)
                      $ v)
          if IS.size x < IS.size y
            then return x
            else return y

-- | Given a vector of elements more and more true for a predicate, give the smallest
-- index such that the predicate is satisfied.
binarySearch ::
  (Monad m)
  => V.Vector e
  -> (e -> m Bool)
  -> m Int
binarySearch vec p =
  go 0 (V.length vec)
  where
    go i j
      | i == j  =
        return i
      | otherwise = do
        let pivot = i + ((j - i) `quot` 2)
        inLowerHalf <- p (vec V.! pivot)
        if inLowerHalf
          then go i pivot
          else go (pivot + 1) j

ddmin ::
     (Monad m, Ord x)
--     (Monad m, MonadIO m, Show x)
  => S.Set x
  -> (S.Set x -> m Bool)
  -- ^ a function can test delta
  -> m(S.Set x)

ddmin world = ddmin' world 2

ddmin' ::
     (Monad m, Ord x)
--     (Monad m, MonadIO m, Show x)
  => S.Set x
  -> Int
  -> (S.Set x -> m Bool)
  -- ^ a function can test delta
  -> m(S.Set x)
ddmin' world n testFunc
  | S.size world == 1 = return world
  | otherwise = do
      firstDelta <- getFirst testFunc deltaSet
      firstDeltaCompl <- getFirst testFunc deltaComplSet
      case (firstDelta, firstDeltaCompl) of
        (Just a, _)        ->
          ddmin' a 2 testFunc
        (Nothing, Just a)  ->
          ddmin' a (max (n-1) 2) testFunc
        (Nothing, Nothing)
          | n < S.size world ->
              ddmin' world (min (S.size world) (2*n)) testFunc
          | otherwise ->
              return world
      where deltaSet      = chopSet n world
            deltaComplSet = S.difference world <$> deltaSet

chopSet :: Ord x => Int -> S.Set x -> [S.Set x]
chopSet n s = go s
  where
    blockSize = length s `roundUpQuot` n
    go s'
      | S.null s' = []
      | otherwise = let (h, r) = S.splitAt blockSize s' in h : go r

roundUpQuot :: Int -> Int -> Int
roundUpQuot i j =
  let (q, r) = quotRem i j in q + if r > 0 then 1 else 0

getFirst ::
    (Foldable t, Monad m)
 => (S.Set x -> m Bool)
 -> t (S.Set x)
 -> m (Maybe (S.Set x))

getFirst testFun =
  foldM fun Nothing
  where
    fun b a = case (b, a) of
                (Nothing, _) -> do
                    rsl <- testFun a
                    if rsl
                    then
                      return $ Just a
                    else
                      return Nothing
                (Just a', _) -> return (Just a')
