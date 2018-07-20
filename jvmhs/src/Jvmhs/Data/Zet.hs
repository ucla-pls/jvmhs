{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-|
Module      : Jvmhs.Data.Zet
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhauge@cs.ucla.edu

A Zet is a special zet of sets, where it is easy to find the smallest
possible union of sets st a property has been uphold.
-}

module Jvmhs.Data.Zet where

import           Control.Lens
import           Control.Monad
-- import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Functor
import qualified Data.IntSet   as IS
import qualified Data.List     as L
import qualified Data.Vector   as V

type Z = IS.IntSet

class Zet z zet | zet -> z where
  -- | From a list of sets, return a Zet.
  fromListOfSet :: [z] -> zet

  -- | Given a predicate, split the search for the smallest set in three.
  -- The left zet contains the search for the smallest z, conditions on z,
  -- The middle set is the smallest z, such that P(LZ \ z).
  -- The right zet, represents the continued search for P(RZ \ z)
  split ::
    MonadPlus m =>
    (z -> m ())
    -> zet
    -> m (zet, z, zet)

  -- | Limit the search for only sets of total size k.
  filter :: Int -> zet -> zet

  -- | Get the total z.
  total :: zet -> z

  emptyZ :: zet -> z
  sizeZ :: zet -> z -> Int

data VectorZet = VectorZet Z (V.Vector Z)

instance Zet IS.IntSet VectorZet where
  fromListOfSet =
    VectorZet IS.empty . V.fromList . L.sortOn IS.size

  split p (VectorZet b z) = do
    i <- binarySearch' (p . IS.union b) (V.postscanl IS.union IS.empty z)
    let midz = (z V.! i) `IS.union` b
    return $
      ( condition (b, V.take i z) midz
      , midz
      , VectorZet b (V.take i z V.++ V.drop (i + 1) z)
      )
    where
      condition (a, mu) z' =
        let VectorZet _ zz = fromListOfSet $ mu ^.. traverse . to (IS.\\ z')
        in VectorZet (a `IS.union` z') zz

  filter k (VectorZet b zz) =
    VectorZet b (V.filter ((<k - bs).IS.size) zz)
    where bs = IS.size b

  total (VectorZet _ zz) =
    V.foldr IS.union IS.empty zz

  emptyZ = const IS.empty
  sizeZ = const IS.size



-- newtype ZZ = ZZ (V.Vector Z)
-- instance Zet ZZ where
--   fromListOfSet zs =
--     ZZ (V.fromList . L.sortOn IS.size $ zs)

--   splitZ p (ZZ z) = do
--     i <- binarySearch' p (V.postscanl IS.union IS.empty z)
--     return $ (ZZ ( V.take i z), (z V.! i), ZZ(V.drop (i + 1) z))

--   conditionZ (ZZ mu) z = fromListOfSet $ mu ^.. traverse . to (IS.union z)
--   filterZ k (ZZ zz) = ZZ (V.filter ((<k).IS.size) zz)
--   mergeZ (ZZ zz1) (ZZ zz2) = (ZZ $ zz1 V.++ zz2)
--   totalZ (ZZ zz) = V.foldr IS.union IS.empty zz



-- -- | A union Zet, is a binary tree of sets, where the branches holds
-- -- the nodes of the sets.
-- data UnionZet
--   = UZBranch Z UnionZet UnionZet
--   | UZNode Z
--   deriving (Show, Eq)

-- newtype CUZ = CUZ (Z, Maybe UnionZet)
--   deriving (Show, Eq)

-- toListZet :: UnionZet -> [Z]
-- toListZet z =
--   go z []
--   where
--     go (UZNode x) = (x:)
--     go (UZBranch _ lz rz) =
--       go lz . go rz

-- -- mergeUZ :: UnionZet -> UnionZet -> UnionZet
-- -- mergeUZ UZEmpty a = a
-- -- mergeUZ a UZEmpty = a

-- mergeUZ :: UnionZet -> UnionZet -> UnionZet
-- mergeUZ lz rz =
--   UZBranch (totalUZ lz `IS.union` totalUZ rz) lz rz

-- mergeMUZ :: Maybe UnionZet -> Maybe UnionZet -> Maybe UnionZet
-- mergeMUZ Nothing a = a
-- mergeMUZ a Nothing = a
-- mergeMUZ (Just x) (Just y) = Just $ mergeUZ x y

-- totalUZ :: UnionZet -> Z
-- totalUZ (UZBranch z _ _) = z
-- totalUZ (UZNode z) = z

-- fromListOfSetUZ :: [IS.IntSet] -> UnionZet
-- fromListOfSetUZ zs =
--   go (L.length zs) $ L.sortOn IS.size zs
--   where
--     go _ [x] = UZNode x
--     go n xs =
--       let
--         p = (n `quot` 2)
--         (l, r) = L.splitAt p xs
--         lz = go p l
--         rz = go (n - p) r
--       in UZBranch (totalUZ lz `IS.union` totalUZ rz) lz rz


-- instance Zet CUZ where
--   fromListOfSet zs =
--     CUZ (IS.empty, Just $ fromListOfSetUZ zs)

--   splitZ _ (CUZ (_, Nothing)) = mzero
--   splitZ p (CUZ (c, Just z)) = do
--     (lz, m, rz) <- go c z
--     return (CUZ (c, lz), m `IS.union` c, CUZ (c, rz))
--     where
--       go b (UZNode x) =
--         p (b `IS.union` x) $> (Nothing, x, Nothing)
--       go b (UZBranch tz lz rz) =
--         p (b `IS.union` tz) >>
--         ( do
--             (lhs, r, rhs) <- go b lz
--             return (lhs, r, rhs `mergeMUZ` Just rz)
--         )
--         <|>
--         ( do
--             (lhs, r, rhs) <- go (b `IS.union` totalUZ lz) rz
--             return (Just lz `mergeMUZ` lhs, r, rhs)
--         )

--   conditionZ (CUZ (c', Nothing)) c = CUZ (c `IS.union` c', Nothing)
--   conditionZ (CUZ (c', Just z)) c = CUZ (c `IS.union` c', Just $ go z)
--     where
--       go (UZNode x) = UZNode (x IS.\\ c)
--       go x@(UZBranch tz lz rz)
--         | IS.null (tz `IS.intersection` c) = x
--         | IS.null (totalUZ rz `IS.intersection` c) =
--           go lz `mergeUZ` rz
--         | otherwise =
--           fromListOfSetUZ [ s IS.\\ c | s <- toListZet x]

--   filterZ _ (CUZ (c, Nothing)) = CUZ (c, Nothing)
--   filterZ k (CUZ (c, Just z)) = CUZ (c, go z)
--     where
--       csize = IS.size c
--       go (UZNode x) =
--         if IS.size x < k - csize then Just (UZNode x) else Nothing
--       go (UZBranch _ lz rz) =
--         case go rz of
--           Nothing -> go lz
--           Just a -> Just $ lz `mergeUZ` a

--   mergeZ (CUZ (c, z)) (CUZ (_, z2)) = CUZ (c, z `mergeMUZ` z2)

--   totalZ (CUZ (_, Nothing)) = IS.empty
--   totalZ (CUZ (c, Just z)) = c `IS.union` totalUZ z


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
