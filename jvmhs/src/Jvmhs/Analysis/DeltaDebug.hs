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
  , sdd'

  , binarySearch
  , binarySearch'
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe

import           Data.Bool
import           Data.Functor

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
  fmap asLabels . sdd' (p.asLabels) $ par
  where
    asLabels = toListOf (folded.toLabel gr._Just) . IS.toList


sdd ::
  (Monad m)
  => ([v] -> m Bool)
  -> [v]
  -> m [v]
sdd p xs =
  unset <$> sdd' (p . unset) _set
  where
    reference = V.fromList xs
    _set = V.fromList [IS.singleton i | i <- [0 .. V.length reference-1]]
    unset = map (reference V.!) . IS.toList

-- | Set delta-debugging
-- Given a list of sets sorted after size, then return the smallest possible
-- set such that uphold the predicate.
sdd' ::
  (Monad m)
  => (IS.IntSet -> m Bool)
  -> V.Vector IS.IntSet
  -> m IS.IntSet
sdd' p v = do
  let mvunion = V.postscanl' IS.union IS.empty v
  Just i <- binarySearch p mvunion
  let s = v V.! i
  t <- p s
  if t
    then pure s
    else do
      x <- sdd' p . V.map (IS.union s) $ V.take i v
      y <- sdd' p (V.fromList
                  . (++ [x])
                  . V.toList
                  . V.ifilter (\j s' -> j /= i && IS.size s' < IS.size x)
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
  unset <$> ddmin' 2 (p . unset) _set
  where
    reference = V.fromList xs
    _set = IS.fromAscList [0 .. V.length reference-1]
    unset = map (reference V.!) . IS.toList

ddmin' ::
  (Monad m)
  => Int
  -> (IS.IntSet -> m Bool)
  -> IS.IntSet
  -> m IS.IntSet
ddmin' n p world
  | worldSize < n =
    pure world
  | otherwise = do
    f <- runMaybeT . msum . concat $
      [ go 2 <$> deltaSet
      , go (max (n-1) 2) <$> deltaComplSet
      ]
    case f of
      Just w ->
        return w
      Nothing
        | n < worldSize ->
          ddmin' (min worldSize (2*n)) p world
        | otherwise ->
          return world
  where
    deltaSet      = chopSet (IS.toAscList world)
    deltaComplSet = IS.difference world <$> deltaSet

    go n' a = do
      guard =<< lift (p a)
      lift $ ddmin' n' p a

    worldSize = IS.size world
    blockSize = worldSize `roundUpQuot` n

    chopSet [] = []
    chopSet s =
      let (h, r) = splitAt blockSize s
      in IS.fromAscList h : chopSet r

roundUpQuot :: Int -> Int -> Int
roundUpQuot i j =
  q + bool 1 0 (r > 0)
  where (q, r) = quotRem i j

binarySearch ::
  (Monad m)
  => (e -> m Bool)
  -> V.Vector e
  -> m (Maybe Int)
binarySearch p =
  runMaybeT . binarySearch' (lift . p >=> guard)

-- | Given a vector of elements more and more true for a predicate, give the smallest
-- index such that the predicate is satisfied.
binarySearch' ::
  (MonadPlus m)
  => (e -> m ())
  -> V.Vector e
  -> m Int
binarySearch' p vec =
  go 0 (V.length vec)
  where
    go i j
      | i == j =
        guard (i < V.length vec) $> i
      | otherwise =
        let pivot = i + ((j - i) `quot` 2)
        in msum
         [ p (vec V.! pivot) >> go i pivot
         , go (pivot + 1) j
         ]
