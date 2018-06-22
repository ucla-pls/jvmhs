{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

  -- * extras
  , sddx
  , MZ
  , ZZ
  , CUZ
  , Zet (..)

  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe

import           Data.Bool
import           Data.Functor
import           Data.Maybe

import qualified Data.IntSet               as IS
import qualified Data.Vector               as V

import           Jvmhs.Data.Graph
import           Jvmhs.Data.Zet

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
  fmap asLabels . sdd' (p.asLabels) . map snd . partition' $ gr
  where
    asLabels = toListOf (folded.toLabel gr._Just) . IS.toList

sdd ::
  (Monad m)
  => ([x] -> m Bool)
  -> [x]
  -> m [x]
sdd =
  sddx (fromListOfSet :: [Z] -> MZ)

sddx ::
  (Monad m, Zet zz)
  => ([IS.IntSet] -> zz)
  -> ([x] -> m Bool)
  -> [x]
  -> m [x]
sddx hlp p xs =
  unset <$> sddx' (p . unset) (hlp _set)
  where
    reference = V.fromList xs
    _set = [IS.singleton i | i <- [0 .. V.length reference-1]]
    unset = map (reference V.!) . IS.toList

{-# SPECIALIZE sddx :: ([IS.IntSet]-> MZ) -> ([x] -> Identity Bool) -> [x] -> Identity [x] #-}
{-# SPECIALIZE sddx :: ([IS.IntSet]-> ZZ) -> ([x] -> Identity Bool) -> [x] -> Identity [x] #-}
{-# SPECIALIZE sddx :: ([IS.IntSet]-> CUZ) -> ([x] -> Identity Bool) -> [x] -> Identity [x] #-}

-- | Set delta-debugging
-- Given a list of sets sorted after size, then return the smallest possible
-- set such that uphold the predicate.
sdd' ::
  (Monad m)
  => (IS.IntSet -> m Bool)
  -> [IS.IntSet]
  -> m IS.IntSet
sdd' predicate ls =
  sddx' predicate (fromListOfSet ls :: MZ)

sddx' ::
  (Monad m, Zet zz)
  => (IS.IntSet -> m Bool)
  -> zz
  -> m IS.IntSet
sddx' predicate ls =
  predicate IS.empty >>= \case
    True ->
      pure IS.empty
    False -> do
      s <- runMaybeT $ go (totalSize) ls
      return $ fromMaybe total s
  where
    total = totalZ ls
    totalSize = IS.size total
    p = lift . predicate >=> guard

    go k mu = do
      (lw, s, hg) <- splitZ p (filterZ k mu)
      p s $> s <|>
        ( do
            subset <- go k (conditionZ lw s)
            go (IS.size subset) (mergeZ lw hg) <|> return subset
        )

{-# SPECIALIZE sddx' :: (Monad m) => (IS.IntSet -> m Bool) -> MZ -> m IS.IntSet #-}
{-# SPECIALIZE sddx' :: (Monad m) => (IS.IntSet -> m Bool) -> ZZ -> m IS.IntSet #-}
{-# SPECIALIZE sddx' :: (Monad m) => (IS.IntSet -> m Bool) -> CUZ -> m IS.IntSet #-}


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
