{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-
Module      : Jvmhs.Analysis.Hierarchy
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : BSD3
Maintainer  : kalhuage@cs.ucla.edu

This module defines the a class hierarchy analysis. Most of the functions in
this module expects a type checking hierarchy.
-}
module Jvmhs.Analysis.Hierarchy
  (
  -- * Hierarchy
  -- $hierarchy
    Hierarchy
  , HasHierarchy (hierarchy)

  -- ** Creation
  , hierarchyFromStubs
  , hierarchyFromStubsWarn

  -- ** Queries
  , HEdge (..)

  , implementations
  , isSubclassOf
  , subclassPath

  -- -- * Hierarchy
  --   Hierarchy (..)
  -- , hryStubs
  -- , hryGraph
  -- , HEdge (..)

  -- , HierarchyStub (..)
  -- , hrySuper
  -- , hryInterfaces
  -- , hryFields
  -- , hryMethods

  -- , HierarchyType (..)

  -- , HierarchyStubs

  -- , toStub
  -- , allStubs
  -- , expandStubs

  -- -- ** Creation
  -- , getHierarchy
  -- , getHierarchyWithStubs
  -- , calculateHierarchy

  -- -- ** Classes
  -- , implementations

  -- -- ** Methods
  -- , definitions
  -- , declaration
  -- , declarations
  -- , isAbstract
  -- , isSubclassOf
  -- , subclassPath
  -- , abstractDeclaration
  -- , callSites
  -- , requiredMethods

  -- -- ** Fields
  -- , fieldLocation

  -- * HierarchyStub
  -- $hierarchyStub

  , HierarchyStub (..)
  , stubIsAbstract, stubSuper, stubInterfaces, stubMethods, stubFields

  -- * Utils
  , IsAbstract
   
  ) where


-- lens
import           Control.Lens

-- aeson
import           Data.Aeson
import           Data.Aeson.TH

-- vector
import qualified Data.Vector as V

-- binary
import Data.Binary

-- mtl
import Control.Monad.Reader

-- transformers
import Control.Monad.Trans.Maybe

-- base
import           Control.Monad
import qualified Data.List as List
import           Data.Functor
import           Control.Applicative
import           Data.Hashable
import           Data.Maybe
import           GHC.Generics        (Generic)

-- containers
import qualified Data.IntSet as IS

-- unordered-containers
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

-- jvmhs
import           Jvmhs.ClassPool
import           Jvmhs.Data.Class
import           Jvmhs.Data.Graph
import           Jvmhs.Data.Type

-- | Decompose takes a 'H.HashMap' and splits it into two parts. A lookup part
-- and a data part.
decompose :: (Eq a, Hashable a) => M.HashMap a b -> (M.HashMap a Int, V.Vector (a, b))
decompose mp = (ifoldMap (\i (a,_) -> M.singleton a i) vector, vector) where
  vector = V.fromList (M.toList mp)

-- | Like decompose, but for sets.
decomposeSet :: (Eq a, Hashable a) => S.HashSet a -> (M.HashMap a Int, V.Vector a)
decomposeSet mp = (ifoldMap (\i a -> M.singleton a i) vector, vector) where
  vector = V.fromList (S.toList mp)

-- | from map utils
fromMap' :: M.HashMap k v -> S.HashSet k
fromMap' m = S.fromMap $ m $> ()


-- $hierarchyStub
-- The goal of the 'HierarchyStub' is to be information that can be
-- saved and composed.

-- | Just a boolean that tracks if the class or method is abstract.
type IsAbstract = Bool

-- | A hierarchy stub, is the only information needed to calculate the
-- hierarchy. The benefit is that this can be loaded up front.
data HierarchyStub = HierarchyStub
  { _stubIsAbstract :: !(IsAbstract)
  , _stubSuper      :: !(Maybe ClassName)
  , _stubInterfaces :: !(S.HashSet ClassName)
  , _stubMethods    :: !(M.HashMap MethodId IsAbstract)
  , _stubFields     :: !(S.HashSet FieldId)
  } deriving (Show, Eq)

makeLenses ''HierarchyStub
$(deriveJSON
  defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 4}
  ''HierarchyStub
  )

-- | Create a 'HierarchyStub' from a 'Class'
toStub :: Class -> HierarchyStub
toStub = do
  _stubIsAbstract <-
    view (classAccessFlags . contains CAbstract)

  _stubSuper <-
    preview (classSuper._Just.classTypeName)

  _stubInterfaces <-
    S.fromList <$> toListOf (classInterfaces.folded.classTypeName)

  _stubMethods <- view
    $ classMethods
    . folded
    . to (liftA2 M.singleton (view methodId) (view (methodAccessFlags . contains MAbstract)))

  _stubFields <- view
    $ classFields . folded
    . fieldId . to S.singleton

  pure $ HierarchyStub {..}

-- | 'HierarchyStubs' is a 'M.HashMap' of stubs.
newtype HierarchyStubs = HierarchyStubs
  { asStubMap :: M.HashMap ClassName HierarchyStub
  } deriving (Show, Eq, Generic, ToJSON, FromJSON, Monoid, Semigroup)

-- | We can write 'HierarchyStubs' to a binary file.
instance Binary HierarchyStubs where
  get = do
    classnames <- getVector getDeserialize
    methodids <- getVector getDeserialize
    fieldids <- getVector getDeserialize
    i <- get
    fmap (HierarchyStubs . M.fromList) . sequence . List.replicate i $ do
      _classname <- seek classnames
      _stubIsAbstract <- get

      _stubSuper <- get <&> \i ->
        if i < 0
        then Nothing
        else Just (classnames V.! i)

      _stubInterfaces <-
        S.fromList <$> getMany (seek classnames)

      _stubMethods <-
        M.fromList <$> getMany (liftA2 (,) (seek methodids) get)

      _stubFields <-
        S.fromList <$> getMany (seek fieldids)

      pure $ (_classname, HierarchyStub {..})

    where
      getMany g = get >>= \i -> sequence $ List.replicate i g

      seek :: V.Vector a -> Get a
      seek a = get <&> \i -> a V.! i

      getVector :: Get a -> Get (V.Vector a)
      getVector g = do
        i <- get
        V.replicateM i g

      getDeserialize :: TextSerializable a => Get a
      getDeserialize =
        either fail return . deserialize =<< get

  put = runReaderT $ do
    (clookup, classnames) <- fmap decomposeSet . view
      $ to asStubMap . to fromMap'
      <> folding asStubMap .
      ( stubSuper._Just.to S.singleton
        <> stubInterfaces
      )

    (mlookup, methodids) <- fmap decomposeSet . view
      $ folding asStubMap.stubMethods.to fromMap'

    (flookup, fieldids) <- fmap decomposeSet . view
      $ folding asStubMap.stubFields

    lift $ do
      putVector putSerialize classnames
      putVector putSerialize methodids
      putVector putSerialize fieldids

    ReaderT $ \a -> iforM_ (asStubMap a) $ \i s -> do
      puts clookup i

      put (s ^. stubIsAbstract)
      case s ^. stubSuper of
        Nothing -> put (-1 :: Int)
        Just a -> puts clookup i
      putsMany (puts clookup) . S.toList $ s ^. stubInterfaces
      putsMany (\(i, a) -> puts mlookup i >> put a) . M.toList $ s ^. stubMethods
      putsMany (puts flookup) . S.toList $ s ^. stubFields

    where
      puts :: (Eq a, Hashable a) => M.HashMap a Int -> a -> Put
      puts l i = put (l M.! i)

      putsMany :: (a -> Put) -> [a] -> Put
      putsMany p lst = do
        put (List.length lst)
        mapM_ p lst

      putVector :: (a -> Put) -> V.Vector a -> Put
      putVector p v = do
        put (V.length v)
        forM_ v p

      putSerialize :: TextSerializable a => a -> Put
      putSerialize =
        put . serialize


-- $hierarchy
--

data Hierarchy = Hierarchy
  { _hierarchyClassIndicies :: !(M.HashMap ClassName Int)
  , _hierarchyItems         :: !(V.Vector HierarchyItem)
  }

type ClassIndex = Int

data HierarchyItem = HierarchyItem
  { _hryClassName       :: !ClassName
  , _hryStub            :: HierarchyStub
  , _hrySuper           :: !(Maybe ClassIndex)
  , _hryInterfaces      :: !(IS.IntSet)
  , _hryImplementations :: IS.IntSet
  -- ^ implementations are intentionally keept lazy so that it can be calulated
  -- on the fly.
  }

makeClassy ''Hierarchy
makeLenses ''HierarchyItem

-- | Calculate the hierarchy and warn about missing items.
hierarchyFromStubsWarn ::
  forall m. Applicative m
  => (ClassName -> m ())
  -- ^ Warning function
  -> HierarchyStubs
  -> m Hierarchy
hierarchyFromStubsWarn tell stubs = do
  initial <- flip traverse items $ \(_hryClassName, _hryStub) -> do
    _hrySuper      <- maybe (pure Nothing) findIdx $ _hryStub ^. stubSuper
    _hryInterfaces <- IS.fromList . catMaybes <$>
      traverse findIdx (_hryStub ^.. stubInterfaces . folded)
    pure $ HierarchyItem {..}

  pure $
    let _hierarchyItems = flip V.imap initial $
          \i -> hryImplementations
            .~ IS.singleton i <> flip foldMap _hierarchyItems
            (\a ->
                if a^.hrySuper == Just i || a^.hryInterfaces.contains i
                then a^.hryImplementations
                else mempty
            )
    in Hierarchy {..}
    
  where
    (_hierarchyClassIndicies, items) = decompose (asStubMap stubs)

    findIdx :: ClassName -> m (Maybe ClassIndex)
    findIdx cn = case M.lookup cn _hierarchyClassIndicies of
      Just idx -> pure (Just idx)
      Nothing -> tell cn $> Nothing

-- | Calculate the hierarchy from stubs.
hierarchyFromStubs :: HierarchyStubs -> Hierarchy
hierarchyFromStubs =
  runIdentity . hierarchyFromStubsWarn (const (pure ()))

type MonadHierarchy env m =
  (MonadReader env m, HasHierarchy env)

classIndex :: MonadHierarchy env m => ClassName -> m (Maybe ClassIndex)
classIndex cn =
  preview (hierarchyClassIndicies.ix cn)

-- | Get a HierarchyItem
item :: MonadHierarchy env m => ClassName -> m (Maybe HierarchyItem)
item cn = classIndex cn >>= unIndicies

-- | Get a HierarchyItem from an ClassIndex
unIndex :: MonadHierarchy env m => ClassIndex -> m HierarchyItem
unIndex i =
  flip V.unsafeIndex i <$> view hierarchyItems

-- | Get a HierarchyItem from an ClassIndex
reClassName :: MonadHierarchy env m => ClassIndex -> m ClassName
reClassName i = unIndex i <&> view hryClassName

unIndicies :: (MonadHierarchy env m, Functor f)
  => f ClassIndex -> m (f HierarchyItem)
unIndicies fci = do
  itms <- view hierarchyItems
  pure $ fmap (V.unsafeIndex itms) fci

-- | Get the stub associated with the 'ClassName'
stub :: MonadHierarchy env m => ClassName -> m (Maybe HierarchyStub)
stub cn = do
  item cn <&> fmap (view hryStub)

-- | Returns a list of all classes that implement some interface, or extends
-- a class, including the class itself.
implementations ::
  MonadHierarchy env m
  => ClassName
  -> m [ClassName]
implementations = item >=> \case
    Just (view hryImplementations -> imps) -> do
      lst <- unIndicies (IS.toList imps)
      return $ map (view hryClassName) lst
    Nothing ->
      return []

infixl 5 `isSubclassOf`
-- | Checks if the first class is a subclass or equal to the second.
isSubclassOf ::
  (MonadHierarchy env m, HasClassName n1, HasClassName n2)
  => n1 -> n2 -> m Bool
isSubclassOf (view className -> cn1) (view className -> cn2) =
  fmap (fromMaybe False) . runMaybeT $ do
  ci1 <- MaybeT $ classIndex cn1
  i2 <- MaybeT $ item cn2

  return $ ci1 `IS.member` (i2^.hryImplementations)

data HEdge
  = Implement
  | Extend
  deriving (Show, Ord, Eq, Enum, Generic)

-- | Finds the path from a subclass A to the superclass B
subclassPath ::
  forall env m. MonadHierarchy env m
  => ClassName
  -> ClassName
  -> m (Maybe [(ClassName, ClassName, HEdge)])
subclassPath cn1 cn2 = runMaybeT $ do
  ci1 <- MaybeT $ classIndex cn1
  ci2 <- MaybeT $ classIndex cn2
  imps <- unIndex ci2 <&> view hryImplementations
  MaybeT . views hierarchy $ findpath imps ci1 ci2

  where
    findpath ::
      IS.IntSet
      -> ClassIndex
      -> ClassIndex
      -> Hierarchy
      -> Maybe [(ClassName, ClassName, HEdge)]
    findpath imps a b hry =
      fmap (map (\(a', b', e) -> (reClassName a' hry, reClassName b' hry, e)))
      $ go a
      where
      go x
        | x == b = return []
        | otherwise = let itm = unIndex x hry in msum
          [ do
              c <- findOf (hrySuper._Just) (`IS.member` imps) itm
              ((a, c, Extend):) <$> go c
          , do
              c <- findOf (hryInterfaces.folding IS.toList) (`IS.member` imps) itm
              ((a, c, Implement):) <$> go c
          ]

-- allStubs ::
--   MonadClassPool m
--   => m HierarchyStubs
-- allStubs =
--    M.fromList <$> mapClasses (\c -> (c^.className, toStub c))

-- expandStubs ::
--   MonadClassPool m
--   => HierarchyStubs
--   -> m HierarchyStubs
-- expandStubs old = do
--   s <- allStubs
--   return $ s <> old


-- instance Hashable HEdge


-- data Hierarchy = Hierarchy
--   { _hryStubs :: HierarchyStubs
--   , _hryGraph :: Graph ClassName HEdge
--   }

-- makeLenses ''Hierarchy

-- getHierarchy ::
--   MonadClassPool m
--   => m ([ClassName], Hierarchy)
-- getHierarchy =
--   getHierarchyWithStubs mempty

-- getHierarchyWithStubs ::
--   MonadClassPool m
--   => HierarchyStubs
--   -> m ([ClassName], Hierarchy)
-- getHierarchyWithStubs =
--   expandStubs >=> return . calculateHierarchy

-- calculateHierarchy ::
--   HierarchyStubs
--   -> ([ClassName], Hierarchy)
-- calculateHierarchy stubs =
--   (missed, Hierarchy stubs (mkGraph nodes edges))
--   where
--     (nodes, edges) = stubs ^. ifolded.withIndex.to collection
--     missed = S.toList (nodes `S.difference` fromMap' stubs)
--     collection (cn, stub) =
--       let toedge x = to (cn,,x) in
--       ( cn `S.insert` view (hrySuper.folded. to S.singleton <> hryInterfaces) stub
--       , S.fromList $ toListOf (hrySuper.folded.toedge Extend <> hryInterfaces.folded.toedge Implement) stub
--       )


-- -- | Return a set of classes that implements a method.
-- definitions ::
--   Hierarchy
--   -> AbsMethodId
--   -> S.HashSet ClassName
-- definitions hry mid =
--   S.fromList
--   . toListOf (folded.filtered (methodDefinedIn hry $ mid^.methodId))
--   . implementations hry
--   $ mid^.className

-- -- | Returns the possible declaration of a method. It might return
-- -- itself
-- declaration ::
--   Hierarchy
--   -> AbsMethodId
--   -> Maybe AbsMethodId
-- declaration hry mid =
--   (\a -> mid & className .~ a) <$> go (mid ^. className)
--   where
--     ii = mid ^. methodId
--     go cn =
--       firstOf
--       ( hryStubs.ix cn .
--         ( hryMethods.ix ii.like cn
--           <> hrySuper.folded.to go._Just
--           <> hryInterfaces.folded.to go._Just
--         )
--       ) hry

-- -- | Checks if the method or any of it's supermethods is declared abstractly.
-- -- This methods stops on the first declared method.
-- abstractDeclaration ::
--   Hierarchy
--   -> AbsMethodId
--   -> Bool
-- abstractDeclaration hry mid =
--   fromMaybe False (declaration hry mid >>= isAbstract hry)

-- -- | Return a list of abstract methods that declares the method, but not the
-- -- method itself. If a method is defined above this, no declaration above this
-- -- is used.
-- declarations :: Hierarchy -> AbsMethodId -> [AbsMethodId]
-- declarations hry m =
--   hry ^.. hryStubs . ix (m ^.className) . folding abstractsInSupers
--   where
--     abstractsInSupers = toListOf $
--       (hrySuper._Just <> hryInterfaces.folded)
--       . folding abstractsInClass

--     abstractsInClass cn =
--       case hry ^. hryStubs.at cn of
--         Just stb ->
--           case stb ^. hryMethods.at mid of
--             Just True -> [mkAbsMethodId cn mid]
--             _ | stb ^. hryType `L.elem` [HInterface, HAbstract]
--                 -> abstractsInSupers stb
--               | otherwise
--                 -> []
--         Nothing -> []

--     mid = m ^. methodId

-- -- | Given a field finds its real location
-- fieldLocation ::
--   Hierarchy
--   -> AbsFieldId
--   -> Maybe AbsFieldId
-- fieldLocation hry mid =
--   (\a -> mid & className .~ a) <$> go (mid ^. className)
--   where
--     ii = mid ^. fieldId
--     go cn =
--       firstOf
--       ( hryStubs.ix cn .
--         ( hryFields.ix ii.like cn
--           <> hrySuper.folded.to go._Just
--           <> hryInterfaces.folded.to go._Just
--         )
--       ) hry

-- isSubclassOf :: Hierarchy -> ClassName -> ClassName -> Bool
-- isSubclassOf hry cn1 cn2 =
--   cn1 `L.elem` implementations hry cn2


-- -- | Finds the path from a subclass A to the superclass B
-- subclassPath :: Hierarchy -> ClassName -> ClassName -> Maybe [(ClassName, ClassName, HEdge)]
-- subclassPath hry a' b = go a'
--   where
--     imps = S.fromList $ implementations hry b
--     go a
--       | a == b = Just []
--       | otherwise =
--         case findOf (hryStubs.ix a.hrySuper._Just) (`S.member` imps) hry of
--           Just c -> ((a, c, Extend):) <$> go c
--           Nothing ->
--             case findOf (hryStubs.ix a.hryInterfaces.folded) (`S.member` imps) hry of
--               Just c -> ((a, c, Implement):) <$> go c
--               Nothing -> Nothing




-- isAbstract :: Hierarchy -> AbsMethodId -> Maybe Bool
-- isAbstract hry mid =
--   hry ^? hryStubs . ix (mid^.className) . hryMethods . ix (mid^.methodId)

-- -- higherMethods :: Hierarchy -> MethodName -> Fold ClassName AbsMethodId
-- -- higherMethods hry =

-- -- | A fold of all abstract super classes, this includes classes and
-- -- intefaces
-- abstractedSuperClasses :: HierarchyStub -> Hierarchy -> [HierarchyStub]
-- abstractedSuperClasses stub h =
--   toListOf (
--     (hrySuper._Just <> hryInterfaces.folded)
--     . folding (\cn -> h ^? hryStubs.ix cn)
--     . filtered (\n -> n ^. hryType `L.elem` [HInterface, HAbstract])
--     ) stub

-- isRequired ::
--   Hierarchy
--   -> AbsMethodId
--   -> Bool
-- isRequired hry m =
--   maybe False isAbstractInSupers $ hry ^. hryStubs.at (m ^.className)
--   where
--     isAbstractInSupers :: HierarchyStub -> Bool
--     isAbstractInSupers stb =
--       or (map hasAbstract $ abstractedSuperClasses stb hry)

--     hasAbstract :: HierarchyStub -> Bool
--     hasAbstract stb =
--       fromMaybe (isAbstractInSupers stb) $ stb ^. hryMethods.at mid

--     mid = m ^. methodId

--   -- where go


-- --   | mid ^. relMethodName.methodId == "<init>:()V" = True
-- --   | otherwise =
-- --     orOf
-- --     (
-- --       hryStubs
-- --       . ix (mid ^. inClassName)
-- --       . ( hrySuper._Just.to (isAbstractIn (mid ^. relMethodName))
-- --         )
-- --     )

-- --     case hry ^.
-- --       Nothing -> False
-- --       Just stub ->
-- --         orOf
-- --         ( hrySuper._Just.to (declaration hry.flip (set inClassName) mid)._Just
-- --         )
-- --         case stub ^?  of
-- --           Just w -> fromMaybe True (isAbstract hry w)
-- --           Nothing ->
-- --             orOf (hryInterfaces.folded.to go) stub
-- --   where
-- --     go cn =
-- --       case hry ^. hryStubs.at cn of
-- --         Nothing -> True
-- --         Just stub ->
-- --           orOf ( hryMethods . ix (mid^. relMethodName)
-- --                  <> hryInterfaces.folded.to go
-- --                ) stub



-- requiredMethods ::
--   Hierarchy
--   -> Class
--   -> [AbsMethodId]
-- requiredMethods hry cls =
--   cls ^.. classAbsMethodIds . filtered (isRequired hry)

-- methodDefinedIn ::
--   Hierarchy
--   -> MethodId
--   -> ClassName
--   -> Bool
-- methodDefinedIn hry mid cn =
--   orOf (hryStubs.at cn._Just.hryMethods.at mid._Just.to not) hry

-- -- | returns a list of possible call sites.
-- callSites ::
--   Hierarchy
--   -> AbsMethodId
--   -> [ AbsMethodId ]
-- callSites hry mid =
--   definitions hry mid ^.. folded . to (\cn -> mid & className .~ cn)

-- -- import qualified Data.Set.Lens       as S
