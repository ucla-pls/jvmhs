{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE ApplicativeDo        #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-|
Module : Jvmhs.Data.Type
Copyright : (c) Christian Gram Kalhauge, 2019
License  : BSD3
Maintainer : kalhauge@cs.ucla.edu

This module describes the types of Jvmhs. The types here contain both the
signatures and anntotations of the jvm-binary package.


Notes: An annotation can be repeated multiple places. For example is an annotation 
on a field both an annotation of its type and the field itself: 
@
  public @A int field; -> A is on field and on type int.
@

-}
module Jvmhs.Data.Type
  (
  -- * Basic types 
    JType(..)
  , _JTRef
  , _JTBase
  , JBaseType(..)
  , JRefType(..)
  , _JTClass
  , _JTArray
  , ClassName(..)

  -- * Bigger types
  , Type(..)
  , _ReferenceType
  , _BaseType
  , ReferenceType(..)
  , _RefClassType
  , _RefArrayType
  , _RefTypeVariable

  -- ** ClassType
  , ClassType(..)
  , classTypeName
  , classTypeArguments
  , classTypeInner
  , extendClassType
  , classTypeFromName
  , innerClassTypeFromName
  , classNameFromType

  -- ** ArrayType
  , ArrayType(..)
  , arrayType

  -- ** ReturnType
  , ReturnType(..)
  , returnType

  -- ** ThrowsType
  , ThrowsType(..)
  , _ThrowsClass
  , _ThrowsTypeVariable
  , classNameFromThrowsType
  , boundClassNameFromThrowsType

  -- ** Others
  , TypeParameter(..)
  , typeIdentifier
  , typeClassBound
  , typeInterfaceBound
  , TypeVariable(..)
  , TypeArgument(..)

  -- * IsSimple
  -- Tests if the types are simple types. This means that there 
  -- are type variables or type applications used.
  , typeIsSimple
  , referenceTypeIsSimple
  , classTypeIsSimple
  , arrayTypeIsSimple
  , annotatedIsSimple
  , returnTypeIsSimple
  , throwsTypeIsSimple

  -- * Annotations
  -- This module uses annotations from the annotations module, we have 
  -- re-exported Annotated here for convinence.
  , Annotated(..)
  , TypePath
  , B.TypePathItem(..)
  , B.TypePathKind(..)
  , setTypeAnnotations
  , getTypeAnnotations

  -- * Helpers
  , fromJType
  , toJType
  , toBoundJType

  -- * Re-exports
  , B.JValue(..)
  , HasTypeAnnotations(..)
  )
where

-- base
import           Data.Either
import           Data.Maybe
import           Data.Foldable
import           GHC.Generics                   ( Generic )

-- deep-seq
import           Control.DeepSeq

-- containers
import           Data.Sequence                  ( (><) )
import qualified Data.Sequence                 as Seq
import qualified Data.Map.Strict               as Map

-- text
import qualified Data.Text                     as Text

-- lens
import           Control.Lens            hiding ( (.=) )

-- jvm-binary
import qualified Language.JVM                  as B
import           Language.JVM.Type
import qualified Language.JVM.Attribute.Annotations
                                               as B

-- jvmhs
import           Jvmhs.Data.Annotation

-- | This is an annotated type paramater, modeled after `B.TypeParameter`.
-- NOTE While the class bound and can technicaly be any reference type, it 
-- is not allowed in the Java language.
data TypeParameter = TypeParameter
  { _typeIdentifier :: ! TypeVariable
  , _typeClassBound     :: ! (Maybe (Annotated ThrowsType))
  , _typeInterfaceBound :: ! [Annotated ThrowsType]
  } deriving (Show, Eq, Generic, NFData)

-- | A reference type can also be Annotated
data ReferenceType
  = RefClassType !ClassType
  | RefTypeVariable !TypeVariable
  | RefArrayType !ArrayType
  deriving (Show, Eq, Generic, NFData)

-- | A throw type can also be annotated
data ThrowsType
  = ThrowsClass !ClassType
  | ThrowsTypeVariable !TypeVariable
  deriving (Show, Eq, Generic, NFData)

-- | An 'ClassType' is interesting because it can represent inner classes
-- in different ways.
data ClassType = ClassType
  { _classTypeName :: ! Text.Text
  , _classTypeInner :: ! (Maybe (Annotated ClassType))
  , _classTypeArguments :: [ Annotated TypeArgument ]
  } deriving (Show, Eq, Generic, NFData)

newtype ArrayType = ArrayType
  { _arrayType :: Annotated Type
  } deriving (Show, Eq, Generic, NFData)

-- | A Return type is a type or void.
newtype ReturnType = ReturnType
  { _returnType :: Maybe Type
  } deriving (Show, Eq, Generic, NFData)

data Type
  = ReferenceType !ReferenceType
  | BaseType !JBaseType
  deriving (Show, Eq, Generic, NFData)

-- | A Type argument is either any type @?@ (@*@), a bounded type 
-- @? extends Object@ (@+java/lang/Object@) or just a type 
-- @Object@ (@java/lang/Object@)
data TypeArgument
  = AnyTypeArg
  | ExtendedTypeArg (Annotated ReferenceType)
  | ImplementedTypeArg (Annotated ReferenceType)
  | TypeArg !ReferenceType
  deriving (Show, Eq, Generic, NFData)

newtype TypeVariable = TypeVariable
  { _typeVarName :: Text.Text
  } deriving (Show, Eq, Generic, NFData)

makeLenses ''ClassType
makeLenses ''ArrayType
makeLenses ''ReturnType
makeLenses ''TypeParameter
makePrisms ''ThrowsType
makePrisms ''ReferenceType
makePrisms ''Type

makePrisms ''JType
makePrisms ''JRefType
makePrisms ''JBaseType

-- | Get the name of a class type, this throws away all anotations and 
-- type signatures
classNameFromType :: ClassType -> ClassName
classNameFromType ct =
  fromRight (error $ "Unexpected behaviour, please report a bug: " ++ show ct)
    $ B.textCls (Text.intercalate "$" $ nameOf ct)
 where
  nameOf t = t ^. classTypeName : maybe
    []
    nameOf
    (t ^? classTypeInner . _Just . annotatedContent)

classNameFromThrowsType :: ThrowsType -> Either TypeVariable ClassName
classNameFromThrowsType = \case
  ThrowsClass        cn -> Right $ classNameFromType cn
  ThrowsTypeVariable tv -> Left tv

-- | Like 'classNameFromThrowsType' but tries to lookup any type variable
-- in an environment first.
boundClassNameFromThrowsType
  :: [TypeParameter] -> ThrowsType -> Either TypeVariable ClassName
boundClassNameFromThrowsType tps =
  either (\v -> typeVariableBound tps v) Right . classNameFromThrowsType


-- | Extend a ClassType with an inner classType.
extendClassType :: ClassType -> ClassType -> ClassType
extendClassType ct =
  classTypeInner
    %~ (Just . \case
         Just (Annotated x a) -> Annotated (extendClassType ct x) a
         Nothing              -> withNoAnnotation ct
       )

-- | Creates a ClassType without any annotations and typesignatures
classTypeFromName :: ClassName -> ClassType
classTypeFromName = innerClassTypeFromName . classNameAsText

-- | Creates a ClassType without any annotations and typesignatures
innerClassTypeFromName :: Text.Text -> ClassType
innerClassTypeFromName = fromJust . go . Text.split (== '$')
 where
  go []       = Nothing
  go (a : as) = Just $ ClassType a (withNoAnnotation <$> go as) []

-- | Convert a Type to either A TypeVariable or a simple type.
toJType :: Type -> Either TypeVariable B.JType
toJType = \case
  ReferenceType rt -> B.JTRef <$> toJRefType rt
  BaseType      bt -> Right $ B.JTBase bt

-- | We can convert a JType to a 'Type' without any annotations or 
-- generics
fromJType :: B.JType -> Type
fromJType = \case
  JTRef  rt -> ReferenceType (fromJRefType rt)
  JTBase bt -> BaseType bt

toBoundJType :: [TypeParameter] -> Type -> Either TypeVariable B.JType
toBoundJType tps =
  either (fmap (B.JTRef . B.JTClass) . typeVariableBound tps) Right . toJType

-- | Create a 'ReferenceType' from a 'JRefType', without any annotations 
-- or generics.
fromJRefType :: B.JRefType -> ReferenceType
fromJRefType = \case
  JTClass cn  -> RefClassType $ classTypeFromName cn
  JTArray atp -> RefArrayType $ ArrayType (withNoAnnotation (fromJType atp))

-- | Convert a ReferenceType to either a 'TypeVariable' or a simple
-- 'B.JRefType'.
toJRefType :: ReferenceType -> Either TypeVariable B.JRefType
toJRefType = \case
  RefClassType ct -> Right $ JTClass (classNameFromType ct)
  RefArrayType (ArrayType (Annotated atp _)) -> JTArray <$> toJType atp
  RefTypeVariable tv -> Left tv


-- | Check if the type is a simple type. This means that there 
-- are no annotations
typeIsSimple :: Type -> Bool
typeIsSimple = \case
  ReferenceType t -> referenceTypeIsSimple t
  BaseType      _ -> True

referenceTypeIsSimple :: ReferenceType -> Bool
referenceTypeIsSimple = \case
  RefClassType    ct  -> classTypeIsSimple ct
  RefTypeVariable _   -> False
  RefArrayType    atp -> arrayTypeIsSimple atp

annotatedIsSimple :: (a -> Bool) -> Annotated a -> Bool
annotatedIsSimple isSimple = view (annotatedContent . to isSimple)

classTypeIsSimple :: ClassType -> Bool
classTypeIsSimple = andOf
  ((classTypeArguments . folded . like False) <> classTypeInner . folded . to
    (annotatedIsSimple classTypeIsSimple)
  )

-- | Check if an ArrayType is simple
arrayTypeIsSimple :: ArrayType -> Bool
arrayTypeIsSimple = view (arrayType . to (annotatedIsSimple typeIsSimple))

throwsTypeIsSimple :: ThrowsType -> Bool
throwsTypeIsSimple = \case
  ThrowsClass        ct -> classTypeIsSimple ct
  ThrowsTypeVariable _  -> False


returnTypeIsSimple :: ReturnType -> Bool
returnTypeIsSimple = andOf (returnType . _Just . to typeIsSimple)

-- | The bound of a type parameter. This is the class that the type
-- parameter will take in code. It is the name of the first bound. 
typeParameterBound
  :: [TypeParameter] -> TypeParameter -> Either TypeVariable ClassName
typeParameterBound tps tp =
  maybe
      (Right "java/lang/Object")
      ( boundClassNameFromThrowsType
          (filter (hasn't $ typeIdentifier . only (tp ^. typeIdentifier)) tps)
      . view annotatedContent
      )
    . firstOf (typeClassBound . folded <> typeInterfaceBound . folded)
    $ tp

typeVariableBound
  :: [TypeParameter] -> TypeVariable -> Either TypeVariable ClassName
typeVariableBound tps tv = case find (has $ typeIdentifier . only tv) tps of
  Just x  -> typeParameterBound tps x
  Nothing -> Left tv



-- updateOrFail
--   :: Traversal' s a
--   -> (a -> Either String a)
--   -> String
--   -> s
--   -> (Either String s)
-- updateOrFail ln fn err s = case s ^? ln of
--   Nothing -> Left err
--   Just a  -> fn a <&> \b -> set ln b s

-- | Redefining '@B.TypePath'
type TypePath = [B.TypePathItem]

-- | Many of the types have annotations, this have 
class HasTypeAnnotations a where
  -- | A path points to a list of annotations or have an error
  typeAnnotations
    :: IndexedTraversal' TypePath a [Annotation]

-- | Get a list of all annotations in the type
getTypeAnnotations :: HasTypeAnnotations a => a -> [(TypePath, Annotation)]
getTypeAnnotations = itoListOf (typeAnnotations <. traverse)

-- | Given a list of annotations set them in the type.
setTypeAnnotations
  :: HasTypeAnnotations a => [(TypePath, Annotation)] -> a -> Either String a
setTypeAnnotations items a =
  let m =
          Map.map toList . Map.fromListWith (><) $ over _2 Seq.singleton <$> items
  in  case
          Map.toList $ m Map.\\ Map.fromList
            (fmap (view (_1 . to (, ()))) $ itoListOf typeAnnotations a)
        of
          [] ->
            Right
              $   a
              &   typeAnnotations
              .@~ (\k -> reverse $ Map.findWithDefault [] k m)
          as -> Left $ "The type does not have the paths " <> show as


instance HasTypeAnnotations a => HasTypeAnnotations (Annotated a) where
  typeAnnotations afb Annotated {..} = do
    _annotatedAnnotations' <- indexed afb
                                      ([] :: [B.TypePathItem])
                                      _annotatedAnnotations
    _annotatedContent' <- typeAnnotations afb _annotatedContent
    pure $ Annotated { _annotatedAnnotations = _annotatedAnnotations'
                     , _annotatedContent     = _annotatedContent'
                     }

instance HasTypeAnnotations ArrayType where
  typeAnnotations =
    reindexed (B.TypePathItem B.TPathInArray 0 :) (arrayType . typeAnnotations)

instance HasTypeAnnotations Type where
  typeAnnotations afb = \case
    ReferenceType f -> ReferenceType <$> typeAnnotations afb f
    a               -> pure a

instance HasTypeAnnotations ReferenceType where
  typeAnnotations afb = \case
    RefClassType f -> RefClassType <$> typeAnnotations afb f
    RefArrayType f -> RefArrayType <$> typeAnnotations afb f
    a              -> pure a

instance HasTypeAnnotations ClassType where
  typeAnnotations afb ClassType {..} = do

    nested <- reindexed (B.TypePathItem B.TPathInNested 0 :)
                        (_Just . typeAnnotations)
                        afb
                        _classTypeInner

    typeargs <- icompose
      (\(i :: Int) j -> B.TypePathItem B.TPathTypeArgument (fromIntegral i) : j)
      itraversed
      typeAnnotations
      afb
      _classTypeArguments

    pure $ ClassType _classTypeName nested typeargs

instance HasTypeAnnotations TypeArgument where
  typeAnnotations afb = \case
    ExtendedTypeArg t' ->
      ExtendedTypeArg
        <$> reindexed (B.TypePathItem B.TPathWildcard 0 :)
                      typeAnnotations
                      afb
                      t'
    ImplementedTypeArg t' ->
      ImplementedTypeArg
        <$> reindexed (B.TypePathItem B.TPathWildcard 0 :)
                      typeAnnotations
                      afb
                      t'
    TypeArg a  -> TypeArg <$> typeAnnotations afb a
    AnyTypeArg -> pure AnyTypeArg

-- | Even though TypeParameter have type annotations they are not
-- accessable through a path. Use the other functions instead.
instance HasTypeAnnotations TypeParameter where
  typeAnnotations _ = pure

instance HasTypeAnnotations ThrowsType where
  typeAnnotations afb = \case
    ThrowsClass ct -> ThrowsClass <$> typeAnnotations afb ct
    a              -> pure a

instance HasTypeAnnotations ReturnType where
  typeAnnotations = returnType . _Just . typeAnnotations
