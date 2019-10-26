{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE RankNTypes                 #-}
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
  , computeHierarchy
  , computeHierarchyWithStubs

  , hierarchyFromStubs
  , hierarchyFromStubsWarn

  -- ** Queries
  , HEdge (..)

  , inStub

  -- ** Classes
  , item
  , super
  , interfaces
  , parents
  , superclasses

  , children
  , implementations
 
  , isSubclassOf
  , isSubclassOf'
  , subclassPath

  -- ** Methods
  , definitions
  , declarations

  -- ** Fields
  , fieldLocations

  -- * HierarchyStub
  -- $hierarchyStub

  , HierarchyStub (..)
  , stubIsAbstract, stubSuper, stubInterfaces, stubMethods, stubFields

  , HierarchyStubs (..)

  -- ** Creation
  , computeStubs
  , computeStubsWithCache

  -- ** IO
  , loadStubs

  -- * Utils
  , IsAbstract
   
  ) where


-- lens
import           Control.Lens hiding (children)

-- aeson
import           Data.Aeson
import           Data.Aeson.TH

-- vector
import qualified Data.Vector as V

-- filepath
import System.FilePath

-- directory
import System.Directory

-- binary
import Data.Binary

-- mtl
import Control.Monad.Reader hiding (fail)
import Control.Monad.Writer hiding (fail)

-- transformers
import Control.Monad.Trans.Maybe

-- base
import Prelude hiding (fail)
import           Data.Functor
import           Data.Foldable
import           Data.Functor.Contravariant
import           Control.Applicative
import           Data.Hashable
import           Control.Monad.Fail
import           Data.Maybe
import           GHC.Generics        (Generic)

-- containers
import qualified Data.IntSet as IS
import qualified Data.IntMap as IM

-- unordered-containers
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S

-- jvmhs
import           Jvmhs.ClassPool
import           Jvmhs.ClassReader
import           Jvmhs.Data.Class
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
  defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 5}
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

compress :: HierarchyStubs -> CompressedStubs
compress = do
  (clookup :: M.HashMap ClassName Int, TextVector -> chsClassNames) <- fmap decomposeSet . view
    $ to asStubMap . to fromMap'
    <> folding asStubMap .
    ( stubSuper._Just.to S.singleton
      <> stubInterfaces
    )

  (mlookup, TextVector -> chsMethodIds) <- fmap decomposeSet . view
    $ folding asStubMap.stubMethods.to fromMap'

  (flookup, TextVector -> chsFieldIds) <- fmap decomposeSet . view
    $ folding asStubMap.stubFields

  chsStubs <- asks $ \a -> flip map (M.toList $ asStubMap a) . uncurry $ \i  -> do
    chsClassId <- pure $ clookup M.! i
    chsIsAbstract <- view stubIsAbstract
    chsSuper <- fmap (clookup M.!) <$> view stubSuper
    chsInterfaces <- IS.fromList <$> toListOf (stubInterfaces.folded.to(clookup M.!))
    chsMethods <- IM.fromList . map (\(m, x) -> (mlookup M.! m, x)) <$> view (stubMethods.to(M.toList))
    chsFields <- IS.fromList . map (flookup M.!) <$> view (stubFields.to(S.toList))
    pure $ CompressedStub {..}

  return $ CompressedStubs {..}

decompress :: CompressedStubs -> HierarchyStubs
decompress (CompressedStubs {..})= do
  HierarchyStubs . M.fromList $
    [ ( chsClassNames ! chsClassId
      , HierarchyStub
        { _stubIsAbstract = chsIsAbstract
        , _stubSuper      = fmap (chsClassNames !) chsSuper
        , _stubInterfaces = S.fromList [ chsClassNames ! i | i <- IS.toList chsInterfaces ]
        , _stubMethods    = M.fromList [ (chsMethodIds ! i, b) | (i, b) <- IM.toList chsMethods ]
        , _stubFields     = S.fromList [ chsFieldIds ! i | i <- IS.toList chsFields ]
        }
      )
    | CompressedStub {..} <- chsStubs
    ]

  where
    (!) :: TextVector a -> Int -> a
    (!) (TextVector v) i = v V.! i


data CompressedStub = CompressedStub
  { chsClassId    :: Int
  , chsIsAbstract :: IsAbstract
  , chsSuper      :: !(Maybe Int)
  , chsInterfaces :: !(IS.IntSet)
  , chsMethods    :: !(IM.IntMap IsAbstract)
  , chsFields     :: !(IS.IntSet)
  } deriving (Generic)

data CompressedStubs = CompressedStubs
  { chsClassNames :: ! (TextVector ClassName)
  , chsMethodIds  :: ! (TextVector MethodId)
  , chsFieldIds   :: ! (TextVector FieldId)
  , chsStubs      :: ! [CompressedStub]
  } deriving (Generic)

newtype TextVector a = TextVector (V.Vector a)
  deriving (Foldable)

instance Binary CompressedStub
instance Binary CompressedStubs

instance TextSerializable a => Binary (TextVector a) where
  get = do i <- get; TextVector <$> V.replicateM i getDeserialize where
    getDeserialize = either fail return . deserialize =<< get
  put (TextVector v) = put (V.length v) >> forM_ v putSerialize where
    putSerialize = put . serialize

-- | We can write 'HierarchyStubs' to a binary file.
instance Binary HierarchyStubs where
  get = decompress <$> get
  put = put . compress

-- | Create HierarchyStubs from all the classes in classPool
allStubs ::
  MonadClassPool m
  => m HierarchyStubs
allStubs =
   HierarchyStubs . M.fromList
   <$> mapClasses (\c -> (c^.className, toStub c))

-- | Expand a set of 'HierarchyStubs' with loaded classes.
expandStubs ::
  MonadClassPool m
  => HierarchyStubs
  -> m HierarchyStubs
expandStubs old = do
  s <- allStubs
  return $ s <> old

-- Compute the stubs from a class reader.
computeStubs :: (MonadIO m, ClassReader r)
  => r
  -> m HierarchyStubs
computeStubs r =
  fst <$> runClassPoolTWithReader
    (const $ allStubs)
    (ReaderOptions False r)

-- Compute the stubs if the cache does not exist. Writes to
-- the cache after the stubs have been loaded.
computeStubsWithCache :: (MonadIO m, ClassReader r)
  => FilePath
  -> r
  -> m HierarchyStubs
computeStubsWithCache fp r = liftIO $ do
  doesFileExist fp >>= \case
    True -> do
      loadStubs fp
    False -> do
      case takeExtension fp of
        ".bin" -> do
          stubs <- computeStubs r
          Data.Binary.encodeFile fp stubs
          return stubs
        ".json" -> do
          stubs <- computeStubs r
          Data.Aeson.encodeFile fp stubs
          return stubs
        _ ->
          error $ "Unknown file format: " <> fp

  where


-- | Load stubs from a file
loadStubs :: (MonadIO m, MonadFail m)
  => FilePath
  -> m HierarchyStubs
loadStubs fp =
  case takeExtension fp of
    ".bin" ->
      liftIO (Data.Binary.decodeFileOrFail fp) >>= \case
        Right a -> return a
        Left msg -> fail (show msg)
    ".json" ->
      liftIO (Data.Aeson.decodeFileStrict' fp) >>= \case
        Just a -> return a
        Nothing -> fail $ "could not load json file: " <> fp
    _ ->
      fail $ "Unknown file format: " <> fp



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
  , _hryInterfaces      :: !IS.IntSet
  , _hryChildren        :: !IS.IntSet
  -- ^ implementations are intentionally keept lazy so that it can be calulated
  -- on the fly.
  -- , _hryChildren        :: IS.IntSet -- The Imediate children
  }

makeClassy ''Hierarchy
makeLenses ''HierarchyItem

-- | Fold over the superclasses of a 'HierarchyItem'.
hryParents :: Fold HierarchyItem ClassIndex
hryParents fn i =
  (hrySuper._Just) fn i *> (hryInterfaces.folding IS.toList) fn i

-- | Fold over the superclasses of a 'HierarchyItem'.
hryAnnotatedParents :: Fold HierarchyItem (ClassIndex, HEdge)
hryAnnotatedParents fn i =
  (hrySuper._Just.to(,Extend)) fn i
  *> (hryInterfaces.folding IS.toList.to(,Implement)) fn i

-- | Fold over the imidiate superclasses of a 'ClassIndex'.
cixParents :: Hierarchy -> Fold ClassIndex ClassIndex
cixParents hry =
  cixItem hry . hryParents

cixClassName :: Hierarchy -> Fold ClassIndex ClassName
cixClassName hry =
  cixItem hry . hryClassName

-- | Fold over the imidiate superclasses of a 'ClassIndex'.
cixAnnotatedParents :: Hierarchy -> Fold ClassIndex (ClassIndex, HEdge)
cixAnnotatedParents hry =
  cixItem hry . hryAnnotatedParents

-- | Fold over the superclasses of a 'ClassIndex'.
cixSuperclasses :: Hierarchy -> Fold ClassIndex ClassIndex
cixSuperclasses hry =
  cosmosOf (cixParents hry)

-- | Fold over the imidiate superclasses of a 'ClassIndex'.
cixChildren :: Hierarchy -> Fold ClassIndex (IS.IntSet)
cixChildren hry =
  cixItem hry . hryChildren

-- | Fold over the imidiate superclasses of a 'ClassIndex'.
cixImplementations :: Hierarchy -> Fold ClassIndex ClassIndex
cixImplementations hry =
  cosmosOf (cixChildren hry . folding IS.toList)


cixItem :: Hierarchy -> Getter ClassIndex HierarchyItem
cixItem hry fn i =
  phantom . fn $ hry ^?! hierarchyItems . ix i


-- | Load all the classes from the classpool. Outputs the hierarchy and a
-- list of missing classes.
computeHierarchy ::
  MonadClassPool m
  => m ([ClassName], Hierarchy)
computeHierarchy =
  computeHierarchyWithStubs mempty

-- | Load all the classes from the classpool.
computeHierarchyWithStubs ::
  MonadClassPool m
  => HierarchyStubs
  -- ^ Add stubs
  -> m ([ClassName], Hierarchy)
computeHierarchyWithStubs stubs = do
  stubs' <- expandStubs stubs
  let (hry, a) = runWriter $ hierarchyFromStubsWarn (tell . Endo . (:)) stubs'
  return (appEndo a [], hry)


-- | Calculate the hierarchy and warn about missing items.
hierarchyFromStubsWarn ::
  forall m. Applicative m
  => (ClassName -> m ())
  -- ^ Warning function
  -> HierarchyStubs
  -> m Hierarchy
hierarchyFromStubsWarn warn stubs = do
  initial <- flip traverse items $ \(_hryClassName, _hryStub) -> do
    _hrySuper      <- maybe (pure Nothing) findIdx $ _hryStub ^. stubSuper
    _hryInterfaces <- IS.fromList . catMaybes <$>
      traverse findIdx (_hryStub ^.. stubInterfaces . folded)
    pure $
      let _hryChildren = mempty
      in HierarchyItem {..}

  pure $
    let
      _children =
        IM.unionsWith IS.union
        . V.toList
        . V.imap
        (\i x ->
            IM.unionsWith IS.union
            ( x ^.. (hryInterfaces.folding (IS.toList) <> hrySuper._Just)
              . to (\j -> IM.singleton j (IS.singleton i))
            )
        )
        $ initial

      _hierarchyItems = flip V.imap initial $ \i a ->
        a { _hryChildren = fold $ IM.lookup i _children }

    --   _hierarchyItems = flip V.imap initial $
    --       \i a ->
    --         case _children IM.!? i of
    --           _ | a ^. hryClassName == "java/lang/Object"
    --               -> a { _hryImplementations = IS.fromList [0..V.length items] }
    --           Just _myChildren ->
    --             a { _hryImplementations = IS.singleton i
    --                 <> foldMap (\i' -> (V.unsafeIndex _hierarchyItems i' ^. hryImplementations))
    --                   (IS.toList _myChildren)
    --               }
    --           Nothing ->
    --             a { _hryChildren = IS.singleton i }
    in Hierarchy {..}

  where
    (_hierarchyClassIndicies, items) = decompose (asStubMap stubs)

    findIdx :: ClassName -> m (Maybe ClassIndex)
    findIdx cn = case M.lookup cn _hierarchyClassIndicies of
      Just idx -> pure (Just idx)
      Nothing -> warn cn $> Nothing

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
inStub :: MonadHierarchy env m => ClassName -> (Getter HierarchyStub a) -> m (Maybe a)
inStub cn l = do
  item cn <&> fmap (view $ hryStub . l)

-- | Returns a list of all classes that implement some interface, or extends
-- a class, including the class itself.
children ::
  MonadHierarchy env m
  => ClassName
  -> m [ClassName]
children = classIndex >=> \case
  Just cix -> views hierarchy $ \hry ->
    toListOf (cixChildren hry.folding IS.toList.cixItem hry.hryClassName) cix
  Nothing ->
    return []

-- | Returns a list of all classes that implement some interface, or extends
-- a class, including the class itself.
implementations ::
  MonadHierarchy env m
  => ClassName
  -> m [ClassName]
implementations = classIndex >=> \case
  Just cix -> views hierarchy $ \hry ->
    toListOf (cixImplementations hry.cixItem hry.hryClassName) cix
  Nothing ->
    return []

infixl 5 `isSubclassOf`
-- | Checks if the first class is a subclass or equal to the second.
isSubclassOf ::
  (MonadHierarchy env m)
  => ClassName -> ClassName -> m Bool
isSubclassOf cn1 cn2 =
  fromMaybe False <$> cn1 `isSubclassOf'` cn2

infixl 5 `isSubclassOf'`
-- | Checks if the first class is a subclass or equal to the second.
isSubclassOf' ::
  (MonadHierarchy env m)
  => ClassName -> ClassName -> m (Maybe Bool)
isSubclassOf' cn1 cn2 = runMaybeT $ do
  ci1 <- MaybeT $ classIndex cn1
  ci2 <- MaybeT $ classIndex cn2
  lift $ superClassSearch ci1 ci2
  -- i2 <- MaybeT $ item cn2
  -- return $ ci1 `IS.member` (i2^.hryImplementations)

superclasses ::
  (MonadHierarchy env m)
  => ClassName -> m [ClassName]
superclasses cn = views hierarchy $ \hry ->
  [ reClassName cix hry
  | cix1 <- maybeToList $ classIndex cn hry
  , cix <- toListOf (cixSuperclasses hry) cix1
  ]

parents ::
  (MonadHierarchy env m)
  => ClassName -> m [ClassName]
parents cn = views hierarchy $ \hry ->
  [ reClassName cix hry
  | cix1 <- maybeToList $ classIndex cn hry
  , cix <- toListOf (cixParents hry) cix1
  ]

super ::
  (MonadHierarchy env m)
  => ClassName -> m (Maybe (Maybe ClassName))
super cn = views hierarchy $ \hry ->
  item cn hry
    <&> preview (hrySuper._Just.cixClassName hry)

interfaces ::
  (MonadHierarchy env m)
  => ClassName -> m (Maybe [ClassName])
interfaces cn = views hierarchy $ \hry ->
  item cn hry
  <&> toListOf (hryInterfaces.folding IS.toList.cixClassName hry)

superClassSearch ::
  (MonadHierarchy env m)
  => ClassIndex -> ClassIndex -> m Bool
superClassSearch ci1 ci2 = views hierarchy $ \hry ->
  anyOf (cixSuperclasses hry) (== ci2) ci1

data HEdge
  = Implement
  | Extend
  deriving (Show, Ord, Eq, Enum, Generic)

-- | Finds the paths from a subclass A to the superclass B
subclassPath ::
  (MonadHierarchy env m)
  => ClassName
  -> ClassName
  -> m [[(ClassName, ClassName, HEdge)]]
subclassPath cn1 cn2 = views hierarchy $ \hry ->
  [ path
  | ci1 <- maybeToList (classIndex cn1 hry)
  , ci2 <- maybeToList (classIndex cn2 hry)
  , path <- findpaths ci1 ci2 hry
  ]

-- | Find all paths from a subclass  to a superclass
findpaths ::
  ClassIndex
  -> ClassIndex
  -> Hierarchy
  -> [[(ClassName, ClassName, HEdge)]]
findpaths a b hry
  | a == b = [[]]
  | otherwise =
    [ (reClassName a hry, reClassName y hry, e) : rest
    | (y, e) <- toListOf (cixAnnotatedParents hry) a
    , rest <- findpaths y b hry
    ]

-- | Given a method id return all defitions of that method. That is all
-- implementations of that method.
-- This method will stop if a method is redefined, but will not
-- report back abstract methods.
definitions ::
  MonadHierarchy env m
  => AbsMethodId
  -> m [AbsMethodId]
definitions mid = views hierarchy $ \hry ->
  [ mid & className .~ cn
  | i <- maybeToList $ item (mid^.className) hry
  , cn <-
    i ^.. hryChildren . folding IS.toList
        . cixItem hry . folding (go (mid^.methodId) hry)
  ]
  where
    go m hry =
      view (hryStub . stubMethods . at m) >>= \case
        Just True -> pure []
        Just False -> toListOf hryClassName
        Nothing ->
          toListOf
            $ hryChildren
            . folding IS.toList
            . cixItem hry
            . folding (go m hry)

-- | Given an absolute method id find all superclasses that declare the
-- method. This method also if that method is declared abstractly.
-- This function also returns itself.
declarations ::
  MonadHierarchy env m
  => AbsMethodId
  -> m [(AbsMethodId, IsAbstract)]
declarations mid = views hierarchy $ \hry ->
  [ result
  | i <- maybeToList $ classIndex (mid^.className) hry
  , result <- i ^.. cixSuperclasses hry.cixItem hry.folding
    (\x ->
       [ (mid & className .~ x^.hryClassName, b)
       | b <- x^.. hryStub.stubMethods.ix (mid^.methodId)
       ]
    )
  ]

-- | Given an absolute field location, return all locations where
-- the field is declared, including itself.
fieldLocations ::
  MonadHierarchy env m
  => AbsFieldId
  -> m [AbsFieldId]
fieldLocations fid = views hierarchy $ \hry ->
  [ fid & className .~ cn
  | i <- maybeToList $ classIndex (fid^.className) hry
  , cn <- i ^..
    cixSuperclasses hry
    . cixItem hry
    . filtered (view $ hryStub.stubFields.contains (fid^.fieldId))
    . hryClassName
  ]

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
