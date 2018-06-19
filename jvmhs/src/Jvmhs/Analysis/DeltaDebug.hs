{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
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
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe

import           Data.Monoid

import qualified Data.IntSet               as IS
import qualified Data.List                 as L
import qualified Data.Vector               as V

import           Jvmhs.Data.Graph

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

-- | Original delta-debugging
ddmin ::
     (Monad m)
  => ([x] -> m Bool)
  -- ^ a function can test delta
  -> [x]
  -> m [x]
ddmin p xs =
  unset <$> ddmin' _set 2
  where
    reference = V.fromList xs
    _set = IS.fromAscList [0 .. V.length reference-1]
    unset = map (reference V.!) . IS.toList
    property = p . unset

    ddmin' world n
      | worldSize == 1 = return world
      | otherwise = do
          firstDelta <- getFirstM property deltaSet
          firstDeltaCompl <- getFirstM property deltaComplSet
          case (firstDelta, firstDeltaCompl) of
            (Just a, _)        ->
              ddmin' a 2
            (Nothing, Just a)  ->
              ddmin' a (max (n-1) 2)
            (Nothing, Nothing)
              | n < worldSize ->
                  ddmin' world (min worldSize (2*n))
              | otherwise ->
                  return world
      where
        worldSize = IS.size world

        deltaSet      = chopSet (IS.toAscList world)
        deltaComplSet = IS.difference world <$> deltaSet

        blockSize = worldSize `roundUpQuot` n

        chopSet [] = []
        chopSet s =
            let (h, r) = splitAt blockSize s
            in IS.fromAscList h : chopSet r

roundUpQuot :: Int -> Int -> Int
roundUpQuot i j =
  let (q, r) = quotRem i j in q + if r > 0 then 1 else 0

getFirstM ::
    (Foldable t, Monad m)
 => (a -> m Bool)
 -> t a
 -> m (Maybe a)
getFirstM testFun =
  runMaybeT . getAlt
    . foldMap (\a -> Alt (lift (testFun a) >>= guard >> return a))

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
