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
  maybe [] unset <$> midd
  where
    midd =
      idd'
        (p.unset) IS.singleton (\k x ls -> if k > IS.size x then V.fromList . reverse $ ls else V.empty)
        V.toList
        _split IS.size (V.length reference)
        (V.imap (const) reference)
    reference = V.fromList xs
    unset = map (reference V.!) . IS.toList
    _split v
      | V.length v == 1 = Left . Just $ V.head v
      | V.length v == 0 = Left Nothing
      | otherwise = Right (V.splitAt (V.length v `quot` 2) v)

idd' ::
  forall m x fx sx. (Monad m, Monoid fx, Show x, Show fx, Show sx)
  => (fx -> m Bool)
  -> (x -> fx)
  -> (Int -> fx -> [x] -> sx)
  -> (sx -> [x])
  -> (sx -> Either (Maybe x) (sx, sx))
  -> (fx -> Int)
  -> Int
  -> sx
  -> m (Maybe fx)
idd' p f reorder items split cost k' s' =
  search k' mempty s'
  where
    search k r s =
      snd <$> go k r [] s

    go ::
      Int -- ^ Maximal cost
      -> fx -- ^ Known values
      -> [x] -- ^ Tested values
      -> sx -- ^ Search space
      -> m ([x], Maybe fx)
      -- ^ Returns a list of tested values and maybe a smallest solution
    go k r c s = do
      -- traceShowM (k, r, c, s)
      -- Test if the search space has any values
      case split s of
        Left Nothing ->
          return (c, Nothing)
        Left (Just x) -> do
          --traceShowM x
          -- If the search space has size 1, then test if it is a solution.
          let r' = f x <> r
          t <- p r'
          -- traceShowM (r', t)
          case t of
            True ->
              -- If yes, it must be the smallest
              return (c, Just r')
            False ->
              -- If no, then search for a subset in the visited solutions,
              -- conditioned on x.
              (c,) <$> search k r' (reorder k r' c)
        Right (bt, tp) -> do
          -- traceShowM (bt, tp)
          let
            bt' = reverse (items bt) ++ c
            r' = (r <> foldMap f bt')
          t <- p r'
          --traceShowM (r', t)
          case t of
            True -> do
              -- Find the minimal solution in the bottom half
              (c', res) <- go k r c bt
              case res of
                -- if there is no solution in the bottom half, search
                -- the top half with knowledge of the c'.
                Nothing ->
                  go k r c' tp
                -- If there is a solution, continue the search with c', but
                -- don't accept solution worse than what we have found.
                Just r'' -> do
                  t' <- p (r <> foldMap f c' <> foldMap f (items tp))
                  if t'
                    then do
                      (_, res') <- go (cost r'') r c' tp
                      return (c', res' <|> res)
                    else
                      return (c', res)
            False -> do
              go k r bt' tp
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
