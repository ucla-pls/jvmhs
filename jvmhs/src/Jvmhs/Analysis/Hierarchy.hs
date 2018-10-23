{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-
Module      : Jvmhs.Analysis.Hierarchy
Copyright   : (c) Christian Gram Kalhauge, 2017
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

This module defines the a class hierarchy analysis. Most of the functions in
this module expects a type checking hierarchy.
-}
module Jvmhs.Analysis.Hierarchy
  (
  -- * Hierarchy
    Hierarchy (..)
  , hryStubs
  , hryGraph

  , HierarchyStub (..)
  , hrySuper
  , hryInterfaces
  , hryFields
  , hryMethods

  , HierarchyStubs

  , toStub
  , allStubs
  , expandStubs

  -- ** Creation
  , getHierarchy
  , getHierarchyWithStubs
  , calculateHierarchy

  -- ** Classes
  , implementations

  -- ** Methods
  , definitions
  , declaration
  , callSites
  , requiredMethods
  ) where


import           Control.Lens

import           Control.Monad
import           Data.Monoid

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Maybe

import           Jvmhs.ClassPool
import           Jvmhs.Data.Class
import           Jvmhs.Data.Graph
import           Jvmhs.Data.Type

import qualified Data.Map         as M
import qualified Data.Set         as S
import qualified Data.Set.Lens    as S

-- | A hierarchy stub, is the only information needed to calculate the
-- hierarchy. The benifit is that this can be loaded up front.
data HierarchyStub = HierarchyStub
  { _hrySuper      :: Maybe ClassName
  , _hryInterfaces :: S.Set ClassName
  , _hryMethods    :: M.Map MethodId Bool
  , _hryFields     :: S.Set FieldId
  } deriving (Show, Eq)

makeLenses ''HierarchyStub
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 4} ''HierarchyStub)

toStub :: Class -> HierarchyStub
toStub cls = HierarchyStub
  (cls ^. classSuper)
  (cls ^. classInterfaces)
  (cls ^. classMethods & traverse %~ views methodCAccessFlags (S.member MAbstract))
  (cls ^. classFields . to M.keysSet)

type HierarchyStubs = M.Map ClassName HierarchyStub

allStubs ::
  MonadClassPool m
  => m HierarchyStubs
allStubs =
   M.fromList . flip appEndo [] . getConst <$>
     traverseClasses (\c -> Const (Endo ((c^.className, toStub c):)))

expandStubs ::
  MonadClassPool m
  => HierarchyStubs
  -> m HierarchyStubs
expandStubs old = do
  s <- allStubs
  return $ s <> old

data HEdge
  = Implement
  | Extend
  deriving (Show, Ord, Eq)

data Hierarchy = Hierarchy
  { _hryStubs :: HierarchyStubs
  , _hryGraph :: Graph ClassName HEdge
  }

makeLenses ''Hierarchy

getHierarchy ::
  MonadClassPool m
  => m ([ClassName], Hierarchy)
getHierarchy =
  getHierarchyWithStubs mempty

getHierarchyWithStubs ::
  MonadClassPool m
  => HierarchyStubs
  -> m ([ClassName], Hierarchy)
getHierarchyWithStubs =
  expandStubs >=> return . calculateHierarchy

calculateHierarchy ::
  HierarchyStubs
  -> ([ClassName], Hierarchy)
calculateHierarchy stubs = do
  (missed, Hierarchy stubs (mkGraph nodes edges))
  where
    (nodes, edges) = stubs ^. ifolded.withIndex.to collection
    missed = S.toList (nodes S.\\ M.keysSet stubs)
    collection (cn, stub) =
      let toedge x = to (cn,,x) in
      ( cn `S.insert` S.setOf (hrySuper.folded <> hryInterfaces.folded) stub
      , S.setOf (hrySuper.folded.toedge Extend <> hryInterfaces.folded.toedge Implement) stub
      )

-- | Returns a list of all classes that implement some interface, or extends
-- a class, including the class itself.
implementations ::
  Hierarchy
  -> ClassName
  -> [ClassName]
implementations hry =
  revReachable (hry ^. hryGraph)

definitions ::
  Hierarchy
  -> AbsMethodId
  -> S.Set ClassName
definitions hry mid =
  S.setOf (folded.filtered (methodDefinedIn hry $ mid^.inId))
    . implementations hry
    $ mid^.inClassName

declaration ::
  Hierarchy
  -> AbsMethodId
  -> Maybe AbsMethodId
declaration hry mid =
  (\a -> mid & inClassName .~ a) <$> go (mid ^. inClassName)
  where
    ii = mid ^. inId
    go cn =
      case hry ^. hryStubs.at cn of
        Nothing -> Nothing
        Just stub ->
          flip firstOf stub $
             (hryMethods.at ii._Just.like cn)
             <> (hrySuper.folded.to go._Just)
             <> (hryInterfaces.folded.to go._Just)

isAbstract :: Hierarchy -> AbsMethodId -> Maybe Bool
isAbstract hry mid =
  hry ^? hryStubs . at (mid^.inClassName) . _Just . hryMethods . at (mid^.inId) ._Just

isRequired ::
  Hierarchy
  -> AbsMethodId
  -> Bool
isRequired hry mid
  | mid ^. inId == "<init>:()V" = True
  | otherwise =
    case hry ^. hryStubs.at (mid ^. inClassName) of
      Nothing -> False
      Just stub ->
        case stub ^? hrySuper._Just.to (declaration hry.flip (set inClassName) mid)._Just of
          Just w -> fromMaybe True (isAbstract hry w)
          Nothing ->
            orOf (hryInterfaces.folded.to go) stub
  where
    go cn =
      case hry ^. hryStubs.at cn of
        Nothing -> True
        Just stub ->
          orOf (hryMethods.at(mid^. inId)._Just <> hryInterfaces.folded.to go) stub

requiredMethods ::
  Hierarchy
  -> Class
  -> [AbsMethodId]
requiredMethods hry cls =
  cls ^.. classMethodIds . filtered (isRequired hry)

methodDefinedIn ::
  Hierarchy
  -> MethodId
  -> ClassName
  -> Bool
methodDefinedIn hry mid cn =
  orOf (hryStubs.at cn._Just.hryMethods.at mid._Just.to not) hry

-- | returns a list of possible call sites.
callSites ::
  Hierarchy
  -> AbsMethodId
  -> [ AbsMethodId ]
callSites hry mid =
  definitions hry mid ^.. folded . to (\cn -> mid & inClassName .~ cn)

-- -- -- | Returns all list of pairs of classes and methods that has
-- -- -- the same id as the method id.
-- -- -- Note: To check if the met
-- -- methodImpls' ::
-- --   MonadClassPool m
-- --   => Hierarchy
-- --   -> AbsMethodId
-- --   -> m [(Class, Method)]
-- -- methodImpls' hry mn = do
-- --   implementations hry (mn ^. inClassName) ^!! traverse.pool._Just.to mpair._Just
-- --   where mpair cls = (cls ^? classMethod (mn ^. inId) . _Just . to (cls,))


-- -- | A HierarchyRecord keeps track of a Class and all it's declared methods, and
-- -- fields.
-- data HierarchyRecord = HierarchyRecord
--   { _hrySuperClasses :: S.Set ClassName
--   -- ^ the set of superclasses, including interfaces.
--   , _hryDeclaredMethods :: M.Map MethodId ClassName
--   -- ^ methods declared by this class, and the class it's declared in
--   , _hryDeclaredFields :: M.Map FieldId ClassName
--   -- ^ fields declared by this class, and the class it's declared in
--   , _hryAbstractMethods :: M.Map MethodId (S.Set ClassName)
--   -- ^ methods not yet implemented, and the class that requires it.
--   } deriving (Show, Eq)

-- instance Monoid HierarchyRecord where
--   mempty = HierarchyRecord mempty mempty mempty mempty
--   mappend a b =
--     HierarchyRecord
--       (on mappend _hrySuperClasses a b)
--       (on mappend _hryDeclaredMethods a b)
--       (on mappend _hryDeclaredFields a b)
--       (on (M.unionWith mappend) _hryAbstractMethods a b)

-- makeLenses ''HierarchyRecord
-- $(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 4} ''HierarchyRecord)

-- -- | A hierarchy is map from 'ClassName's to 'HierarchyRecord's
-- newtype Hierarchy = Hierarchy
--   { _hryAsClassMap :: M.Map ClassName HierarchyRecord }
--   deriving (Show, Eq, ToJSON, FromJSON)


-- makeLenses ''Hierarchy

-- expandHierarchy ::
--   MonadClassPool m
--   => Hierarchy
--   -> m ([ClassName], Hierarchy)
-- expandHierarchy hry = do
--   cs <- allClasses
--   let
--     mp = M.fromList $ cs ^.. traverse.to (\c -> (c^.className, c))
--     classmap = M.map snd r <> hry ^. hryAsClassMap
--     totalmissing = S.unions [ S.fromList m | (m, _) <- M.elems r ]
--     r :: M.Map ClassName ([ClassName], HierarchyRecord)
--     r = flip M.map mp $ \cls ->
--       let
--         parents = dependencies cls
--         (missing, records) = parents ^. folded
--                            . to (\x -> maybe ([x],mempty) ([],) (x `M.lookup` classmap))
--         abstractMethods =
--           M.toMapOf
--             (classMethods
--             .ifolded
--             .filtered (views methodCAccessFlags (S.member MAbstract))
--             .like (S.singleton $ cls^.className)) cls
--         declaredMethods =
--           M.toMapOf
--             (classMethods
--             .ifolded
--             .filtered (views methodCAccessFlags (not . (S.member MAbstract)))
--             .like (cls^.className)) cls
--         fields = M.toMapOf (classFields.ifolded.like (cls^.className)) cls
--       in (missing, HierarchyRecord
--            (S.fromList parents `S.union` (records ^. hrySuperClasses) )
--            (declaredMethods `M.union` (records ^. hryDeclaredMethods) )
--            (fields `M.union` (records ^. hryDeclaredFields) )
--            (abstractMethods `M.union`
--               (M.withoutKeys (records ^. hryAbstractMethods) $ M.keysSet declaredMethods))
--            )
--   return $ (S.toList totalmissing, Hierarchy classmap)

-- calculateHierarchy ::
--   MonadClassPool m
--   => m ([ClassName], Hierarchy)
-- calculateHierarchy = do
--   expandHierarchy (Hierarchy mempty)

-- --   (classNames, hierachy) <- clss ^! folded . pool ._Just . to collection
-- --   let
-- --     (nodes_, nodeMap) = mkNodes new classNames
-- --     Just edges_ = mkEdges nodeMap hierachy
-- --   return $ Hierarchy nodeMap (mkGraph nodes_ edges_)
-- --   where
-- --     collection cls =
-- --       let toedge x = to (cls^.className,,x) in
-- --       ( toListOf (className <> classSuper <> classInterfaces.folded) cls
-- --       , toListOf (classSuper.toedge Extend <> classInterfaces.folded.toedge Implement) cls
-- --       )

-- -- | Canonical method id
-- canonicalMethodId ::
--   Hierarchy
--   -> AbsMethodId
--   -> Maybe AbsMethodId
-- canonicalMethodId hry amid =
--   let cls = hry
--         ^? hryAsClassMap . at (amid^.inClassName)
--           . _Just . hryDeclaredMethods . at (amid^.inId) . _Just
--   in (\s -> amid & inClassName .~ s) <$> cls


-- -- | For each method id, have a map from classnames to classes that has a methodid
-- -- of that type and implments the method named in the class.
-- data ReverseHierarchy = ReverseHierarchy
--   { _revhryImplementations ::
--       M.Map ClassName (S.Set ClassName)
--   , _revhryDeclarations ::
--       M.Map MethodId (M.Map ClassName (S.Set ClassName))
--   } deriving (Show, Eq)

-- makeLenses ''ReverseHierarchy

-- -- | Given a hierarchy, reverse it so that questions like callSites and such
-- -- makes sense. A reverse hierarchy is not expandable.
-- reverseHierarchy ::
--   Hierarchy -> ReverseHierarchy
-- reverseHierarchy hry =
--   ReverseHierarchy implementations declarations
--   where
--     implementations =
--       foldBy (M.unionWith mappend) mempty $
--         hry ^.. hryAsClassMap
--               . ifolded . withIndex
--               . to (\(cn, dc) -> M.toMapOf (hrySuperClasses.folded.ito (,S.singleton cn)) dc)
--     declaredMethods :: M.Map MethodId (S.Set ClassName)
--     definedMethods =
--       M.toMapOf (
--              hryAsClassMap.ifolded
--            . hryDeclaredMethods. to (M.toAscList).folded
--            . withIndex . filtered (\(cn, (m, cn')) -> cn == cn')._2.to swap
--            ) hry
--     declaredMethods :: M.Map MethodId (S.Set ClassName)
--     abstractdMethods =
--       M.toMapOf (
--              hryAsClassMap.ifolded
--            . hryAbstractMethods. to (M.toAscList).folded
--            . withIndex . filtered (\(cn, (m, cn')) -> cn == cn')._2.to
--            ) hry
--     declarations =
--       declaredMethods <> abstractMethods)^.folded.




-- -- nodeOf :: Hierarchy -> ClassName -> Node
-- -- nodeOf hry = fst . mkNode_ (_hryNodes hry)

-- -- -- | Given a Hierarchy, determine the parrents of a class.
-- -- -- The returned list is in order as they appear in the class hierarchy.
-- -- parrents ::
-- --      Hierarchy
-- --   -> ClassName
-- --   -> [ClassName]
-- -- parrents hry@(Hierarchy _ graph) cln =
-- --   let search = dfs [nodeOf hry cln] graph
-- --   in tail $ search ^.. folded . to (lab graph) . _Just

-- -- -- | Given a Hierarchy, determine the superclasses of a class.
-- -- -- The returned list is in order as they appear in the class hierarchy.
-- -- superclasses ::
-- --      Hierarchy
-- --   -> ClassName
-- --   -> [ClassName]
-- -- superclasses hry@(Hierarchy _ graph) cln =
-- --   let search = dfs [nodeOf hry cln] (elfilter (==Extend) graph)
-- --   in tail $ search ^.. folded . to (lab graph) . _Just

-- -- -- | Returns a list of all classes that implement some interface, or extends
-- -- -- a class, including the class itself.
-- -- implementations ::
-- --   Hierarchy
-- --   -> ClassName
-- --   -> [ClassName]
-- -- implementations hry@(Hierarchy _ graph) cln =
-- --   rdfs [nodeOf hry cln] graph ^.. folded . to (lab graph) . _Just

-- -- -- | Get the class name of the containing class of a field id.
-- -- -- This is delegates to 'fieldFromId'.
-- -- classNameOfFieldId ::
-- --   MonadClassPool m
-- --   => AbsFieldId
-- --   -> m (Maybe ClassName)
-- -- classNameOfFieldId fid =
-- --   fmap (view (_1.className)) <$> fieldFromId fid

-- -- -- | Get the class in which the field resides. The function searches from an
-- -- -- initial class.
-- -- fieldFromId ::
-- --   MonadClassPool m
-- --   => AbsFieldId
-- --   -> m (Maybe (Class, Field))
-- -- fieldFromId afid = go (afid ^. inClassName)
-- --   where
-- --     fid = afid ^. inId
-- --     go "java.lang.Object" =
-- --       return Nothing
-- --     go cn = do
-- --       mc <- getClass cn
-- --       case mc of
-- --         Nothing -> return Nothing
-- --         Just cls -> do
-- --           case cls ^. classField fid of
-- --             Nothing -> go (cls^.classSuper)
-- --             a -> return . fmap (cls,) $ a


-- -- -- | Get the class name of the containing class of a method id.
-- -- -- This is delegates to 'methodFromId'.
-- -- classNameOfMethodId ::
-- --   MonadClassPool m
-- --   => AbsMethodId
-- --   -> m (Maybe ClassName)
-- -- classNameOfMethodId mid =
-- --   fmap (view (_1.className)) <$> methodFromId mid

-- -- | Get the class name and the method of the method id. Starting
-- -- the search from a class, and proceeds through.
-- methodFromId ::
--   MonadClassPool m
--   => AbsMethodId
--   -> m (Maybe (Class, Method))
-- methodFromId amid = do
--   go $ amid ^. inClassName
--   where
--     mid = amid ^. inId
--     go cn = do
--       mc <- getClass cn
--       case mc of
--         Nothing -> return Nothing
--         Just cls ->
--           case cls ^. classMethod mid of
--             Nothing ->
--               case cls^.classSuper of
--                 Just cn' -> go cn'
--                 Nothing -> return Nothing
--             a -> return . fmap (cls,) $ a



-- -- -- | Returns all list of pairs of classes and methods that has
-- -- -- the same id as the method id.
-- -- -- Note: To check if the met
-- -- methodImpls' ::
-- --   MonadClassPool m
-- --   => Hierarchy
-- --   -> AbsMethodId
-- --   -> m [(Class, Method)]
-- -- methodImpls' hry mn = do
-- --   implementations hry (mn ^. inClassName) ^!! traverse.pool._Just.to mpair._Just
-- --   where mpair cls = (cls ^? classMethod (mn ^. inId) . _Just . to (cls,))

-- -- -- | Like 'methodImpls'' with the extra check that all the methods has code
-- -- -- executable.
-- -- methodImpls ::
-- --   MonadClassPool m
-- --   => Hierarchy
-- --   -> AbsMethodId
-- --   -> m [(Class, Method)]
-- -- methodImpls hry mn =
-- --   toListOf (folded.filtered(has $ _2.methodCode._Just)) <$> methodImpls' hry mn

-- -- isImplementation ::
-- --   MonadClassPool m
-- --   => Hierarchy
-- --   -> AbsMethodId
-- --   -> m Bool
-- -- isImplementation hry mid = do
-- --   mm <- mid ^!? inClassName.pool._Just. classMethod (mid ^. inId) . _Just
-- --   case mm of
-- --     Just method
-- --       | has (methodCode._Just) method -> do
-- --         let clss = parrents hry (mid ^. inClassName)
-- --         Any res <- clss ^! folded . pool
-- --           . to (maybe (Any True)
-- --                (view $ classMethod (mid ^. inId) . like (Any True)))
-- --         return res
-- --     _ ->
-- --       return False

-- -- isMethodRequired ::
-- --   MonadClassPool m
-- --   => Hierarchy
-- --   -> AbsMethodId
-- --   -> m Bool
-- -- isMethodRequired hry mid =
-- --   fmap (maybe False (const True)) . runMaybeT $
-- --     msum
-- --       [ guard (mid ^. inId . methodIdName == "<clinit>")
-- --       , guard =<< lift (isImplementation hry mid)
