{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase       #-}
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
import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe

import           Data.Bool
import           Data.Maybe
import           Data.Functor

import           Debug.Trace

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
  let par = L.sortOn IS.size . map snd $ partition' gr
  fmap asLabels . sdd' (p.asLabels) $ par
  where
    asLabels = toListOf (folded.toLabel gr._Just) . IS.toList


sdd ::
  (Monad m)
  => ([x] -> m Bool)
  -> [x]
  -> m [x]
sdd p xs =
  unset <$> sdd' (p . unset) _set
  where
    reference = V.fromList xs
    _set = [IS.singleton i | i <- [0 .. V.length reference-1]]
    unset = map (reference V.!) . IS.toList

-- | Set delta-debugging
-- Given a list of sets sorted after size, then return the smallest possible
-- set such that uphold the predicate.
sdd' ::
  (Monad m)
  => (IS.IntSet -> m Bool)
  -> [IS.IntSet]
  -> m IS.IntSet
sdd' predicate ls =
  predicate IS.empty >>= \case
    True ->
      pure IS.empty
    False -> do
      s <- runMaybeT $ calcMuAndGo totalSize IS.empty vs
      return $ fromMaybe total s
  where
    vs = V.fromList ls
    total = V.foldr IS.union IS.empty vs
    totalSize = IS.size total
    p = lift . predicate >=> guard

    calcMu k known v = mu
      where
        mu = V.generate vi $ \i -> (bool known (mu V.! (i - 1)) (i > 0)) `IS.union` (v V.! i)
        vi = fromMaybe (V.length v).runIdentity $ binarySearch (pure.(k<).IS.size) v

    calcMuAndGo k known v =
      go k known v (calcMu k known v)

    -- The recursive function,
    --   k is the max size of set
    --   known is the set of values known to be in the set
    --   v is the vector to be searched for sets
    go k known v mu = do
      -- A binary search over the union of the remaining sets and the known set.
      i <- binarySearch' p mu

      -- s is the smallest set where the moving union satisfies the predicate
      let s = (v V.! i `IS.union` known)

      -- If the predicate satisfies the smallest set, then great!
      p s $> s <|>
        -- else find the subset that satisfies the predicated
        ( do
            subset <- go k s (V.take i v) (IS.union s <$> V.take i mu)
            -- check if a smaller set exists that does not contain the damming elements
            -- from s.
            calcMuAndGo (IS.size subset) known (V.ifilter (\j _ -> j /= i) v)
              <|> return subset
        )

-- class SetSet a where
--   split :: Monad m => (IS.IntSet -> m Bool) -> a -> m (a, IS.IntSet, a)
--   condition :: IS.IntSet -> a -> a
--   merge :: a -> a -> a

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
  q + bool 0 1 (r > 0)
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
