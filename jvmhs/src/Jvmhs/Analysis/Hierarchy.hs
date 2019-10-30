{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE BlockArguments             #-}
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
  , SubclassPath (..), (<:), (<!)
  , subclassEdges

  , inStub

  -- ** Classes
  -- , item
  , super
  , interfaces
  , parents
  , superclasses

  , children
  , implementations
  , implementationPaths
 
  , isSubclassOf
  , isSubclassOf'
  , subclassPaths

  -- ** Methods
  , definitions
  , declarations
  , declaredMethods

  , superDefinitionPaths
  , superAbstractDeclarationPaths

  -- ** Fields
  , fieldLocations
  , fieldLocationPaths

  -- * HierarchyStub
  -- $hierarchyStub

  , HierarchyStub (..)
  , stubIsAbstract, stubSuper, stubInterfaces, stubMethods, stubFields

  , HierarchyStubs (..)

  -- ** Creation
  , computeStubs
  , computeStubsWithCache

  , allStubs
  , expandStubs

  -- ** IO
  , loadStubs

  -- * Utils
  , IsAbstract
   
  ) where


-- lens
import           Control.Lens hiding (children, (...))

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

-- base
import Prelude hiding (fail)
import           Data.Functor
import           Data.Function
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

-- | Expand a set of 'HierarchyStubs' with loaded classes, will
-- overload any new stubs.
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

-- | A Path is a list of subclass relationships.
data SubclassPath
  = SubclassOf !ClassName !HEdge SubclassPath
  | Top !ClassName
  deriving (Eq, Ord)

instance Show SubclassPath where
  showsPrec n cp =
    case cp of
      Top cn ->
        showParen (n >= 9) $ showString "Top ". showsPrec 9 cn
      SubclassOf cn Implement rest ->
        showParen (n > 5) $
        showsPrec 5 cn
        . showString " <: "
        . showsPrec 5 rest

      SubclassOf cn Extend rest ->
        showParen (n > 5) $
        showsPrec 5 cn
        . showString " <! "
        . showsPrec 5 rest
                          

infixr 5 <:
-- | Short for `flip SubclassOf Implement`
(<:) :: ClassName -> SubclassPath -> SubclassPath
(<:) = flip SubclassOf Implement

infixr 5 <!
-- | Short for `flip SubclassOf Extend`
(<!) :: ClassName -> SubclassPath -> SubclassPath
(<!) = flip SubclassOf Extend

bottomOf :: SubclassPath -> ClassName
bottomOf = \case
  Top cn -> cn
  SubclassOf cn _ _ -> cn

subclassEdges :: SubclassPath -> [(ClassName, ClassName, HEdge)]
subclassEdges = \case
  SubclassOf cn e sc ->
    (cn, bottomOf sc, e) : subclassEdges sc
  Top _ ->
    []

  

-- | A HEdge is describing whether
data HEdge
  = Implement
  | Extend
  deriving (Show, Ord, Eq, Enum, Generic)


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
  , _hryChildrenMap     :: !(IM.IntMap HEdge)
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

-- | Fold over the superclasses of a 'HierarchyItem'.
hryChildren :: Fold HierarchyItem ClassIndex
hryChildren = hryChildrenMap . folding IM.keys

-- | Fold over the superclasses of a 'HierarchyItem'.
hryAnnotatedChildren :: Fold HierarchyItem (ClassIndex, HEdge)
hryAnnotatedChildren = hryChildrenMap . folding IM.toList

-- | Fold over the imidiate superclasses of a 'ClassIndex'.
cixParents :: Hierarchy -> Fold ClassIndex ClassIndex
cixParents hry =
  cixItem hry . hryParents

cixClassName :: Hierarchy -> Fold ClassIndex ClassName
cixClassName hry =
  cixItem hry . hryClassName

-- -- | Fold over the imidiate superclasses of a 'ClassIndex'.
-- cixAnnotatedParents :: Hierarchy -> Fold ClassIndex (ClassIndex, HEdge)
-- cixAnnotatedParents hry =
--   cixItem hry . hryAnnotatedParents

-- | Fold over the superclasses of a 'ClassIndex'.
cixSuperclasses :: Hierarchy -> Fold ClassIndex ClassIndex
cixSuperclasses hry =
  cosmosOf (cixParents hry)

-- | Fold over the imidiate superclasses of a 'ClassIndex'.
cixChildren :: Hierarchy -> Fold ClassIndex ClassIndex
cixChildren hry =
  cixItem hry . hryChildren

-- -- | Fold over the imidiate superclasses of a 'ClassIndex'.
-- cixAnnotatedChildren :: Hierarchy -> Fold ClassIndex (ClassIndex, HEdge)
-- cixAnnotatedChildren hry =
--   cixItem hry . hryAnnotatedChildren

-- | Fold over the imidiate superclasses of a 'ClassIndex'.
cixImplementations :: Hierarchy -> Fold ClassIndex ClassIndex
cixImplementations hry =
  cosmosOf (cixChildren hry)

cixItem :: Hierarchy -> Getter ClassIndex HierarchyItem
cixItem hry fn i =
  phantom . fn $ hry ^?! hierarchyItems . ix i

-- cixStub :: Hierarchy -> Getter ClassIndex HierarchyStub
-- cixStub hry =
--   cixItem hry. hryStub

cnIndex ::
  Hierarchy -> Getter ClassName (Maybe ClassIndex)
cnIndex hry fn cn =
  phantom . fn $ hry ^? hierarchyClassIndicies . ix cn

cnItem ::
  Hierarchy -> Fold ClassName HierarchyItem
cnItem hry =
  cnIndex hry._Just.cixItem hry

cnStub ::
  Hierarchy -> Fold ClassName HierarchyStub
cnStub hry =
  cnItem hry.hryStub

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

newtype SemigroupIntMap a = SemigroupIntMap
  { runSemigroupIntMap :: IM.IntMap a }

instance Semigroup s => Semigroup (SemigroupIntMap s) where
  a <> b = SemigroupIntMap $ (IM.unionWith (<>) `on` runSemigroupIntMap) a b

instance Semigroup s => Monoid (SemigroupIntMap s) where
  mempty = SemigroupIntMap (mempty)

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
      let _hryChildrenMap = mempty
      in HierarchyItem {..}

  pure $
    let
      _children = runSemigroupIntMap
        $ view (ifolded . withIndex . to (uncurry collectChildren)) initial

      collectChildren :: ClassIndex -> HierarchyItem -> SemigroupIntMap (IM.IntMap HEdge)
      collectChildren idx = views
        (hryInterfaces.folding IS.toList.to (,Implement) <> hrySuper._Just.to (,Extend))
        \(i, e) -> SemigroupIntMap (IM.singleton i (IM.singleton idx e))

      _hierarchyItems = flip V.imap initial $ \i a ->
        a { _hryChildrenMap = fold $ IM.lookup i _children }

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

-- | Get the stub associated with the 'ClassName'
inStub :: MonadHierarchy env m => ClassName -> (Getter HierarchyStub a) -> m (Maybe a)
inStub cn l = views hierarchy $ \hry -> do
  cn ^? cnStub hry . l

-- | Returns a list of all classes that implement some interface, or extends
-- a class, including the class itself.
children ::
  MonadHierarchy env m
  => ClassName
  -> m [(ClassName, HEdge)]
children cn = views hierarchy $ \hry ->
  [ (i ^?! cixClassName hry, edge)
  | (i, edge) <- cn^..cnItem hry.hryAnnotatedChildren
  ]

-- | Returns a list of all classes that implement some interface, or extends
-- a class, including the class itself.
implementations ::
  MonadHierarchy env m
  => ClassName
  -> m [ClassName]
implementations cn = views hierarchy $ \hry ->
  toListOf
  ( cnIndex hry . _Just
  . cixImplementations hry
  . cixClassName hry
  ) cn

-- | Returns a list of implementations classes, if they are abstract and
-- the path to get there.
implementationPaths ::
  MonadHierarchy env m
  => ClassName
  -> m [(ClassName, IsAbstract, SubclassPath)]
implementationPaths cn = views hierarchy $ \hry ->
  [ (cn', isAbstract, path)
  | it <- cn ^.. cnItem hry
  , (cn', isAbstract, path) <- implementationPaths' it hry
  ]

implementationPaths' ::
  HierarchyItem
  -> Hierarchy
  -> [(ClassName, IsAbstract, SubclassPath)]
implementationPaths' it' hry = go (Top $ it'^.hryClassName) it' where
  go p it =
    (it^.hryClassName, it^.hryStub.stubIsAbstract, p) : concat
    [ go (SubclassOf (item^.hryClassName) edge p) item
    | (chidx, edge) <- it ^.. hryAnnotatedChildren
    , let item = chidx ^. cixItem hry
    ]

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
isSubclassOf' cn1 cn2 = views hierarchy $ \hry -> do
  ci1 <- cn1 ^. cnIndex hry
  ci2 <- cn2 ^. cnIndex hry
  Just (superClassSearch ci1 ci2 hry)
  -- i2 <- MaybeT $ item cn2
  -- return $ ci1 `IS.member` (i2^.hryImplementations)

superclasses ::
  (MonadHierarchy env m)
  => ClassName -> m [ClassName]
superclasses cn = views hierarchy $ \hry ->
  toListOf (cnIndex hry._Just.cixSuperclasses hry.cixClassName hry) cn

parents ::
  (MonadHierarchy env m)
  => ClassName -> m [ClassName]
parents cn = views hierarchy $ \hry ->
  toListOf (cnIndex hry._Just.cixParents hry.cixClassName hry) cn

super ::
  (MonadHierarchy env m)
  => ClassName -> m (Maybe (Maybe ClassName))
super cn = views hierarchy $ \hry ->
  (cn ^? cnItem hry) <&> preview (hrySuper._Just.cixClassName hry)

interfaces ::
  (MonadHierarchy env m)
  => ClassName -> m (Maybe [ClassName])
interfaces cn = views hierarchy $ \hry ->
  (cn ^? cnItem hry)
  <&> toListOf (hryInterfaces.folding IS.toList.cixClassName hry)

superClassSearch ::
  (MonadHierarchy env m)
  => ClassIndex -> ClassIndex -> m Bool
superClassSearch ci1 ci2 = views hierarchy $ \hry ->
  anyOf (cixSuperclasses hry) (== ci2) ci1

-- | Finds the paths from a subclass A to the superclass B
subclassPaths ::
  (MonadHierarchy env m)
  => ClassName
  -> ClassName
  -> m [SubclassPath]
subclassPaths cn1 cn2 = views hierarchy $ \hry ->
  [ path
  | ci1 <- cn1 ^.. cnIndex hry._Just
  , ci2 <- cn2 ^.. cnIndex hry._Just
  , path <- findpaths ci1 ci2 hry
  ] where
  -- | Find all paths from a subclass to a superclass
  findpaths a' trg hry = go a' where
    go a
      | a == trg = [Top (a^?!cixClassName hry)]
      | otherwise =
        let it = a ^. cixItem hry in
        [ SubclassOf (it^.hryClassName) e path
        | (y, e) <- it ^.. hryAnnotatedParents
        , path <- go y
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
  | cn <-
    mid ^.. className.cnItem hry
    . hryChildren
    . cixItem hry . folding (go (mid^.methodId) hry)
  ] where
  go m hry = view (hryStub . stubMethods . at m) >>= \case
    Just True -> pure []
    Just False -> toListOf hryClassName
    Nothing ->
      toListOf
        $ hryChildren
        . cixItem hry
        . folding (go m hry)

-- | Finds all defintions above a methodid.
superDefinitionPaths ::
  AbsMethodId
  -> Hierarchy
  -> [(AbsMethodId, SubclassPath)]
superDefinitionPaths m = views hierarchy $ \hry ->
  m^..className.cnItem hry.folding (superDefs hry)
  where
    mid = m^.methodId
    superDefs hry = go where
      go item = case item ^. hryStub.stubMethods.at mid of
        Just isAbstract ->
          [ (m & className .~ item^.hryClassName, Top $ item^.hryClassName)
          | not isAbstract
          ]
        Nothing ->
          [ (mx, SubclassOf (item^.hryClassName) edge path)
          | (y, edge) <- item ^.. hryAnnotatedParents
          , (mx, path) <- go (y^?!cixItem hry)
          ]

-- | Finds all declarations above the method, including itself.
superAbstractDeclarationPaths ::
  AbsMethodId
  -> Hierarchy
  -> [(AbsMethodId, SubclassPath)]
superAbstractDeclarationPaths m = views hierarchy $ \hry ->
  m^..className.cnItem hry.folding (superDefs hry)
  where
    mid = m^.methodId
    superDefs hry = go where
      go item = 
        [ (m & className .~ item^.hryClassName, Top $ item^.hryClassName)
        | True <- item ^.. hryStub.stubMethods.ix mid
        ] ++
        [ (mx, SubclassOf (item^.hryClassName) edge path)
        | (y, edge) <- item ^.. hryAnnotatedParents
        , (mx, path) <- go (y^?!cixItem hry)
        ]

-- | Given an absolute method id find all superclasses that declare the
-- method. This method also if that method is declared abstractly.
-- This function also returns itself.
declarations ::
  MonadHierarchy env m
  => AbsMethodId
  -> m [(AbsMethodId, IsAbstract)]
declarations mid = views hierarchy $ \hry ->
  mid ^.. className. cnIndex hry._Just
    .cixSuperclasses hry.cixItem hry.folding
    (\x ->
       [ (mid & className .~ x^.hryClassName, b)
       | b <- x^.. hryStub.stubMethods.ix (mid^.methodId)
       ]
    )

-- Returns a list of declared methods and if they are abstract or not.
declaredMethods ::
  MonadHierarchy env m
  => ClassName
  -> m (M.HashMap MethodId (ClassName, IsAbstract))
declaredMethods cn = views hierarchy $ \hry ->
  cn ^. cnItem hry . to (go hry)
  where
    go :: Hierarchy
      -> HierarchyItem
      -> M.HashMap MethodId (ClassName, IsAbstract)
    go hry i = flip view i $
      hryStub . stubMethods . to(fmap (i^.hryClassName,))
      <> hryParents . cixItem hry . to (go hry)
 

-- | Given an absolute field location, return all locations where
-- the field is declared, including itself.
fieldLocations ::
  MonadHierarchy env m
  => AbsFieldId
  -> m [AbsFieldId]
fieldLocations fid = views hierarchy $ \hry ->
  [ fid & className .~ cn
  | cn <- fid ^.. className
    . cnIndex hry._Just
    . cixSuperclasses hry
    . cixItem hry
    . filtered (view $ hryStub.stubFields.contains (fid^.fieldId))
    . hryClassName
  ]

-- | Find the posible locations of fields above this field declaration. It will
-- stop when the first parents field have been detected.
-- INFO: This does not take field overloading into account.
fieldLocationPaths ::
  AbsFieldId
  -> Hierarchy
  -> [(AbsFieldId, SubclassPath)]
fieldLocationPaths f = views hierarchy $ \hry ->
  f^..className.cnItem hry.folding (superDefs hry)
  where
    fid = f^.fieldId
    superDefs hry = go where
      go item = case item ^. hryStub.stubFields.contains fid of
        True ->
          [ (f & className .~ item^.hryClassName, Top $ item^.hryClassName) ]
        False ->
          [ (fx, SubclassOf (item^.hryClassName) edge path)
          | (y, edge) <- item ^.. hryAnnotatedParents
          , (fx, path) <- go (y^?!cixItem hry)
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
