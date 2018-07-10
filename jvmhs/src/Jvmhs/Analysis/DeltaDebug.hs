{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE TupleSections    #-}
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
  (
    DeltaDebugger
  , SetDeltaDebugger
  , GraphDeltaDebugger

  , ddmin
  , gddmin

  , idd
  , igdd
  , iidd
  , isdd

  , zdd
  , zgdd
  , zsdd

  , Zet.binarySearch
  , Zet.binarySearch'

  -- * extras
  , Zet.VectorZet
  -- , Zet.Zet

  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe

import           Data.Bool
import           Data.Functor
import           Data.Maybe
import           Data.Monoid

import Debug.Trace

import qualified Data.IntSet               as IS
import qualified Data.List                 as L
import qualified Data.Vector               as V

import           Jvmhs.Data.Graph
import qualified Jvmhs.Data.Zet as Zet

type DeltaDebugger =
  forall x m. (Monad m) => ([x] -> m Bool) -> [x] -> m [x]

type SetDeltaDebugger =
  forall m. (Monad m) => (IS.IntSet -> m Bool) -> [IS.IntSet] -> m IS.IntSet

type GraphDeltaDebugger =
  forall x e m. (Monad m, Ord x) => ([x] -> m Bool) -> Graph x e -> m [x]

desetdd :: SetDeltaDebugger -> DeltaDebugger
desetdd dd p xs =
  unset <$> dd (p . unset) (map IS.singleton [0 .. V.length reference-1])
  where
    reference = V.fromList xs
    unset = map (reference V.!) . IS.toList
{-# INLINE desetdd #-}

desetgdd :: SetDeltaDebugger -> GraphDeltaDebugger
desetgdd dd p gr =
  fmap asLabels . dd (p.asLabels) . map snd . partition' $ gr
  where
    asLabels = toListOf (folded.toLabel gr._Just) . IS.toList
{-# INLINE desetgdd #-}


-- The Original DeltaDebugger

-- | Original delta-debugging
ddmin :: DeltaDebugger
ddmin p xs =
  unset <$> ddmin' 2 (p . unset) _set
  where
    reference = V.fromList xs
    _set = IS.fromAscList [0 .. V.length reference-1]
    unset = map (reference V.!) . IS.toList

-- | Ddmin which upholds the graph requirements
gddmin ::
     (Monad m, Ord x)
  => ([x] -> m Bool)
  -> Graph x e
  -> m [x]
gddmin p gr =
  ddmin predicate (gr ^.. grNodes)
  where
    predicate x =
      if x `isClosedIn` gr
      then p x
      else return False

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


-- Zet SetDeltaDebugger

zdd :: DeltaDebugger
zdd = desetdd zsdd

-- | Set delta-debugging
-- Given a list of sets sorted after size, then return the smallest possible
-- set such that uphold the predicate.
zsdd :: SetDeltaDebugger
zsdd predicate ls =
  corezdd predicate (Zet.fromListOfSet ls :: Zet.VectorZet)

zgdd :: GraphDeltaDebugger
zgdd =
  desetgdd zsdd

corezdd ::
  (Monad m, Zet.Zet z zz)
  => (z -> m Bool)
  -> zz
  -> m z
corezdd predicate ls =
  predicate (_empty) >>= \case
    True ->
      pure _empty
    False -> do
      s <- runMaybeT $ go (Zet.sizeZ ls total) ls
      return $ fromMaybe total s
  where
    _empty = Zet.emptyZ ls
    total = Zet.total ls
    p = lift . predicate >=> guard

    go k mu = do
      (lw, s, hg) <- Zet.split p (Zet.filter k mu)
      p s $> s <|>
        ( do
            subset <- go k lw
            go (Zet.sizeZ lw subset) hg <|> return subset
        )

{-# INLINE corezdd #-}

-- Search SetDeltaDebugger

idd :: DeltaDebugger
idd p xs =
  maybe xs unset <$> runMaybeT midd
  where
    midd =
      coreidd
        (predicate.unset)
        (IS.null)
        (const $ V.fromList . IS.toList)
        (IS.fromAscList . V.toList)
        _split IS.size (V.length reference)
        (V.imap (const) reference)
    predicate = lift . p >=> guard
    reference = V.fromList xs
    unset = map (reference V.!) . IS.toList
    _split i v
      | i == 0 = mzero
      | V.length v == 1 = return . Left . IS.singleton $ V.head v
      | otherwise = return $ Right (V.splitAt (V.length v `quot` 2) v)

isdd :: SetDeltaDebugger
isdd p xs =
  maybe total fst <$> runMaybeT midd
  where
    total = (IS.unions xs)
    midd =
      coreidd
        (predicate . fst)
        (IS.null . fst)
        (\r -> fromListOfSets . L.map (flip IS.difference (fst r)) . snd)
        ((\v -> (IS.unions v, v)) . V.toList)
        _split (IS.size . fst) (IS.size total)
        (fromListOfSets $ xs)
    predicate = lift . p >=> guard

    fromListOfSets =
      V.fromList . L.sortOn IS.size

    _split ::
         forall m. MonadPlus m
      => Int
      -> V.Vector IS.IntSet
      -> m (Either (IS.IntSet, [IS.IntSet]) (V.Vector IS.IntSet, V.Vector IS.IntSet))
    _split i v' =
      let v = V.filter ((i >=) . IS.size) v'
      in msum
      [ do guard (V.length v == 1)
           return . Left . (\x -> (x, [x])) $ V.head v
      , do guard (V.length v > 1)
           return $ Right (V.splitAt (V.length v `quot` 2) v)
      ]

iidd :: DeltaDebugger
iidd = desetdd isdd

igdd :: GraphDeltaDebugger
igdd = desetgdd isdd

coreidd ::
  forall m x sx. (MonadPlus m, Monoid x, Show x, Show sx)
  => (x -> m ())
     -- ^ The predicate to test with
  -> (x -> Bool)
     -- ^ Check if a solution is empty
  -> (x -> x -> sx)
     -- ^ Turn a solution into a search space, conditioned on a solution
  -> (sx -> x)
     -- ^ Turn a search space into solution
  -> (Int -> sx -> m (Either x (sx, sx)))
     -- ^ Given a maximal size of a single element split the search space into
     -- pieces. If nothing exists in the search space fail with mzero.
  -> (x -> Int)
     -- ^ A cost function
  -> Int
     -- ^ A maximal cost
  -> sx
     -- ^ The search space
  -> m x
coreidd p isnull fromsol tosol split cost k' s' = do
  snd <$> go k' mempty mempty s'
  where
    testngo k r c s = do
      guard $ k > cost r
      p (c <> tosol s <> r)
      go k r c s

    go k r c s = do
      -- Split the search space
      res <- split (k - cost r) s
      case res of
        Left x ->
          -- If there only exist one element
          let r' = x <> r in msum
            [ -- then return r' if there is nothing in the searched values
              guard (isnull c) $> (mempty, r')

            , -- there is something in the already searched values, test
              -- if the single value is enough
              p r' $> (c,r')

            , -- else search the already searched values for a smaller set
              do
                guard $ k > cost r'
                (_, r'') <- go k r' mempty (fromsol r' c)
                return (c, r'')
            ]

        Right (bt, tp) ->
          -- If it is possible to split the search space
          msum
            [ -- test if the solution is in the bottom part of the search space
              do
                (c', r') <- testngo k r c bt
                -- if a solution was found, search the top part for a smaller solution
                testngo (cost r' - 1) r c' tp <|> return (c', r')

            , -- else search the top part, with the bottom part added to the
              -- already searched values
              go k r (c <> tosol bt) tp
            ]
{-# INLINE coreidd #-}

roundUpQuot :: Int -> Int -> Int
roundUpQuot i j =
  q + bool 0 1 (r > 0)
  where (q, r) = quotRem i j
