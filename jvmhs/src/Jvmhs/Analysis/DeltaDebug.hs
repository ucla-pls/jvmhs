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
  ( ddmin
  , sdd
  , sdd'

  , gdd
  , gddmin

  , idd'
  , idd

  , Zet.binarySearch
  , Zet.binarySearch'

  -- * extras
  , sddx
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
import qualified Data.Vector               as V

import           Jvmhs.Data.Graph
import qualified Jvmhs.Data.Zet as Zet

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
  sddx $ (Zet.fromListOfSet :: [IS.IntSet] -> Zet.VectorZet) . map IS.singleton

sddx ::
  (Monad m, Zet.Zet IS.IntSet zz)
  => ([Int] -> zz)
  -> ([x] -> m Bool)
  -> [x]
  -> m [x]
sddx hlp p xs =
  unset <$> sddx' (p . unset) (hlp [0 .. V.length reference-1])
  where
    reference = V.fromList xs
    unset = map (reference V.!) . IS.toList

{-# SPECIALIZE sddx :: ([Int] -> Zet.VectorZet) -> ([x] -> Identity Bool) -> [x] -> Identity [x] #-}

-- | Set delta-debugging
-- Given a list of sets sorted after size, then return the smallest possible
-- set such that uphold the predicate.
sdd' ::
  (Monad m)
  => (IS.IntSet -> m Bool)
  -> [IS.IntSet]
  -> m IS.IntSet
sdd' predicate ls =
  sddx' predicate (Zet.fromListOfSet ls :: Zet.VectorZet)

sddx' ::
  (Monad m, Zet.Zet z zz)
  => (z -> m Bool)
  -> zz
  -> m z
sddx' predicate ls =
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

{-# SPECIALIZE sddx' :: (Monad m) => (IS.IntSet -> m Bool) -> Zet.VectorZet -> m IS.IntSet #-}

idd ::
  (Monad m)
  => ([x] -> m Bool)
  -> [x]
  -> m [x]
idd p xs =
  maybe xs unset <$> runMaybeT midd
  where
    midd =
      idd'
        (predicate.unset)
        IS.singleton
        (V.fromList . reverse)
        V.toList
        _split IS.size (V.length reference)
        (V.imap (const) reference)
    predicate = lift . p >=> guard
    reference = V.fromList xs
    unset = map (reference V.!) . IS.toList
    _split i v
      | i == 0 = mzero
      | V.length v == 1 = return . Left $ V.head v
      | otherwise = return $ Right (V.splitAt (V.length v `quot` 2) v)

idd' ::
  forall m x fx sx. (MonadPlus m, Monoid fx, Show x, Show fx, Show sx)
  => (fx -> m ())
  -> (x -> fx)
  -> ([x] -> sx)
  -> (sx -> [x])
  -> (Int -> sx -> m (Either x (sx, sx)))
  -> (fx -> Int)
  -> Int
  -> sx
  -> m fx
idd' p f reorder items split cost k' s' = do
  search k' mempty s'
  where
    search k r s =
      snd <$> go k r [] s

    test r c s =
      p (r <> foldMap f c <> foldMap f (items s))

    testngo k r c s = do
      guard $ k > cost r
      test r c s
      go k r c s

    go ::
      Int -- ^ Maximal cost
      -> fx -- ^ Known values
      -> [x] -- ^ Tested values
      -> sx -- ^ Search space
      -> m ([x], fx)
      -- ^ Returns a list of tested values and maybe a smallest solution
    go k r c s = do
      -- Test if any solution exists in this space
      -- traceShowM (k, r, c, s)
      res <- split (k - cost r) s
      case res of
        Left x ->
          let r' = f x <> r
          in case c of
            [] ->
              return ([] , r')
            _ ->
              ((c,r') <$ p r') <|> ((c,) <$> search k r' (reorder c))
        Right (bt, tp) ->
          ( do
               (c', r') <- testngo k r c bt
               testngo (cost r' - 1) r c' tp <|> (return (c', r'))
          ) <|> go k r (reverse (items bt) ++ c) tp
{-# INLINABLE idd' #-}

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

roundUpQuot :: Int -> Int -> Int
roundUpQuot i j =
  q + bool 0 1 (r > 0)
  where (q, r) = quotRem i j
