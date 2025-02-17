{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-
Module      : Jvmhs.TypeCheck
Copyright   : (c) Christian Gram Kalhauge, 2018
License     : MIT
Maintainer  : kalhuage@cs.ucla.edu

-}
module Jvmhs.TypeCheck (
  TypeInfo (..),
  _TRef,
  _TBase,
  TBase (..),
  _TInt,
  _TLong,
  _TDouble,
  _TFloat,
  AsTypeInfo (..),
  -- , AsLocalType (..)
  typecheck,
  TypeChecker,
  Checkable (..),
  meet,
  typeCheck,
  -- , typeCheckDebug
  debugInfo,
  TypeCheckState (..),
  HasTypeCheckState (..),
  _JTArray,
  _JTClass,
  _Single,
) where

-- base
import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Data.Foldable
import Data.Function
import qualified Data.List as List
import GHC.Generics (Generic)

-- containers
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IS

-- lens
import Control.Lens

-- aeson
import Data.Aeson hiding ((.=))
import Data.Aeson.TH

-- mtl
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

-- vector
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

-- jvm-binary
import qualified Language.JVM as B

-- jvmhs
import Jvmhs.Analysis.Hierarchy
import Jvmhs.Data.Code
import Jvmhs.Data.Identifier
import Jvmhs.Data.Type

data TBase
  = TInt
  | TLong
  | TFloat
  | TDouble
  deriving (Show, Eq, Ord, Generic)

data TypeInfo
  = TBase !TBase
  | -- | Empty list means null
    TRef ![JRefType]
  | -- | All types
    TTop
  deriving (Show, Eq, Ord, Generic)

makePrisms ''TBase
makePrisms ''TypeInfo

type Instruction = B.ByteCodeOpr B.High

-- class AsLocalType a where
--   asLocalType :: a -> LocalType

--   default asLocalType :: AsTypeInfo a => a -> LocalType
--   asLocalType = asLocalType . asTypeInfo

-- instance AsLocalType TypeInfo where
--   asLocalType = \case
--     TBase TInt  -> LInt
--     TBase TLong     -> LLong
--     TBase TFloat    -> LFloat
--     TBase TDouble   -> LDouble
--     TRef _ -> LRef
--     TTop -> error "Cannot convert Top to LocalType"

-- instance AsLocalType ArrayType where

instance AsTypeInfo B.LocalType where
  asTypeInfo = \case
    B.LInt -> TBase $ TInt
    B.LLong -> TBase $ TLong
    B.LFloat -> TBase $ TFloat
    B.LDouble -> TBase $ TDouble
    B.LRef -> TRef []

instance AsTypeInfo B.ArrayType where
  asTypeInfo = \case
    B.AByte -> TBase $ TInt
    B.AChar -> TBase $ TInt
    B.AShort -> TBase $ TInt
    B.AInt -> TBase $ TInt
    B.ALong -> TBase $ TLong
    B.AFloat -> TBase $ TFloat
    B.ADouble -> TBase $ TDouble
    B.ARef -> TRef []

{- | Since there are many types in the Java eco system It would be nice to all
 cast them to a single type system.
-}
class AsTypeInfo a where
  asTypeInfo :: a -> TypeInfo

instance AsTypeInfo TypeInfo where
  asTypeInfo = id

instance AsTypeInfo ClassName where
  asTypeInfo = TRef . (: []) . JTClass

instance AsTypeInfo TBase where
  asTypeInfo = TBase

instance AsTypeInfo B.JRefType where
  asTypeInfo = TRef . (: [])

instance AsTypeInfo B.JBaseType where
  asTypeInfo =
    TBase . \case
      JTByte -> TInt
      JTChar -> TInt
      JTDouble -> TDouble
      JTFloat -> TFloat
      JTShort -> TInt
      JTBoolean -> TInt
      JTInt -> TInt
      JTLong -> TLong

instance AsTypeInfo B.JType where
  asTypeInfo = \case
    JTBase b -> asTypeInfo b
    JTRef a -> asTypeInfo a

instance AsTypeInfo (Maybe B.JValue) where
  asTypeInfo = \case
    Nothing -> TRef []
    Just a -> asTypeInfo a

-- instance AsLocalType ArithmeticType

instance AsTypeInfo B.ArithmeticType where
  asTypeInfo =
    TBase . \case
      B.MInt -> TInt
      B.MLong -> TLong
      B.MFloat -> TFloat
      B.MDouble -> TDouble

instance AsTypeInfo B.JValue where
  asTypeInfo = \case
    B.VInteger _ -> TBase TInt
    B.VLong _ -> TBase TLong
    B.VFloat _ -> TBase TFloat
    B.VDouble _ -> TBase TDouble
    B.VString _ -> TRef [JTClass "java/lang/String"]
    B.VClass _ -> TRef [JTClass "java/lang/Class"]
    B.VMethodType _ -> TRef [JTClass "java/lang/invoke/MethodType"]
    B.VMethodHandle _ -> TRef [JTClass "java/lang/invoke/MethodHandle"]

class Checkable a b where
  checkEq :: a -> b -> Bool

-- instance AsTypeInfo b => Checkable TypeInfo b where
--   checkEq a b = a == asTypeInfo b

-- instance AsTypeInfo b => Checkable JType b where
--   checkEq a b = asTypeInfo a == asTypeInfo b

instance (AsTypeInfo a, AsTypeInfo b) => Checkable a b where
  checkEq a b = asTypeInfo a == asTypeInfo b

-- instance AsTypeInfo b => Checkable LocalType b where
--   checkEq a b = a == asLocalType b

-- instance AsLocalType b => Checkable ArrayType b where
--   checkEq a b = asLocalType a == asLocalType b

-- instance AsLocalType b => Checkable ArithmeticType b where
--   checkEq a b = asLocalType a == asLocalType b

data TypeCheckState = TypeCheckState
  { _tcStack :: [TypeInfo]
  , _tcLocals :: IntMap.IntMap TypeInfo
  , _tcNexts :: [B.ByteCodeIndex]
  }
  deriving (Show, Eq)

data TypeCheckError
  = EmptyStack
  | NotEqual String String
  | NoLocal B.LocalAddress
  | BadType TypeInfo
  | UnexpectedTypeError String
  | NotSubtype TypeInfo TypeInfo
  | NotIntersect TypeInfo TypeInfo
  | InconsistentStates B.ByteCodeIndex TypeCheckState TypeCheckState
  deriving (Show, Eq)

instance Exception TypeCheckError

type TypeChecker m =
  ( MonadState TypeCheckState m
  , MonadReader Hierarchy m
  , MonadError TypeCheckError m
  )

makeClassy ''TypeCheckState

getLocal :: TypeChecker m => B.LocalAddress -> m TypeInfo
getLocal addr = do
  let idx = fromIntegral addr
  use (tcLocals . at idx) >>= \case
    Just l -> return l
    Nothing -> throwError (NoLocal addr)

putLocal :: (TypeChecker m, Show a, AsTypeInfo a) => a -> B.LocalAddress -> m ()
putLocal a i = do
  tcLocals . at (fromIntegral i) .= Just (asTypeInfo a)

pop :: TypeChecker m => m TypeInfo
pop =
  use tcStack >>= \case
    [] -> throwError EmptyStack
    a : rest -> do
      tcStack .= rest
      return a

push :: TypeChecker m => AsTypeInfo t => t -> m ()
push t = tcStack %= (asTypeInfo t :)

-- currentByteCodeIndex :: TypeChecker ByteCodeIndex
-- currentByteCodeIndex =
--   use tcCurrentByteCodeIndex

addNext :: TypeChecker m => B.ByteCodeIndex -> m ()
addNext n = tcNexts %= (n :)

setNext :: TypeChecker m => B.ByteCodeIndex -> m ()
setNext n = noNext >> addNext n

noNext :: TypeChecker m => m ()
noNext = tcNexts .= []

isSubtypeOf :: (MonadReader Hierarchy m) => TypeInfo -> TypeInfo -> m Bool
isSubtypeOf = curry $ \case
  (TRef as, TRef bs)
    | as == bs ->
        return True
    | otherwise -> asks $
        \r -> and [(a `isSubReftypeOf` b) r | a <- toList as, b <- toList bs]
  (_, TTop) -> return True
  (a, b) -> return (a == b)

-- | Checks if a is S is a subtype of T.
isSubReftypeOf :: (MonadReader Hierarchy m) => JRefType -> JRefType -> m Bool
isSubReftypeOf = \case
  JTClass s -> \case
    JTClass t -> isSubclassOf s t
    _ -> return False
  JTArray s -> \case
    JTArray t -> case (s, t) of
      (JTRef s', JTRef t') -> s' `isSubReftypeOf` t'
      _ -> return $ s == t
    JTClass t ->
      return $
        List.elem
          t
          ["java/lang/Object", "java/lang/Cloneable", "java/io/Serializable"]

unpack :: TypeChecker m => (APrism' TypeInfo a) -> TypeInfo -> m a
unpack p ti = case ti ^? clonePrism p of
  Just x -> return x
  Nothing -> throwError (BadType ti)

-- Return the type of array execpt if it the typeinfo is null in which case
-- we return Nothing
isArray :: TypeChecker m => TypeInfo -> m TypeInfo
isArray ti =
  case foldl
    (\a b -> a >>= meet (asTypeInfo b))
    (Just TTop)
    (ti ^.. _TRef . folded . _JTArray) of
    Just x -> return x
    Nothing -> throwError (BadType ti)

-- check ::
--   (AsTypeInfo a, AsTypeInfo b, Show a, Show b, MonadError TypeCheckError m)
--   => a -> b -> m ()
-- check a b
--   | checkEq a b =
--     return ()
--   | otherwise =
--     throwError (NotEqual (show a) (show b))

infixl 5 `checkSubtypeOf`

{- | Checks if two types are equal or if A can be cast to B.
 This means A is a subclass of B
-}
checkSubtypeOf :: (AsTypeInfo a, AsTypeInfo b, TypeChecker m) => a -> b -> m ()
checkSubtypeOf a b =
  unlessM (asTypeInfo a `isSubtypeOf` asTypeInfo b) $
    throwError (NotSubtype (asTypeInfo a) (asTypeInfo b))

-- infixl 5 `checkIntersectOf`
-- checkIntersectOf ::
--   (AsTypeInfo a, AsTypeInfo b, TypeChecker m)
--   => a -> b -> m ()
-- checkIntersectOf a b =
--   unlessM
--   ( liftM2 (||)
--     (asTypeInfo a `isSubtypeOf` asTypeInfo b)
--     (asTypeInfo b `isSubtypeOf` asTypeInfo a)
--   ) $ throwError (NotIntersect (asTypeInfo a) (asTypeInfo b))

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mbool munit =
  mbool >>= \case
    True -> return ()
    False -> munit

-- whenM :: Monad m => m Bool -> m () -> m ()
-- whenM = unlessM . (not <$>)

typeCheck
  :: Hierarchy
  -- ^ A class hierarchy.
  -> AbsMethodId
  -- ^ the absolute name of the method to check
  -> Bool
  -- ^ is the method static
  -> Code
  -- ^ the code to check
  -> ( Maybe (B.ByteCodeIndex, TypeCheckError)
     , V.Vector TypeCheckState
     )
typeCheck hry mn isStatic code = V.createT $ do
  entries <- VM.replicate (V.length $ code ^. codeByteCode) defaultState
  runExceptT
    ( dfs entries (IS.fromList [0]) -- :code^..codeExceptionTable.folded.ehHandler))
    )
    <&> \case
      Right () -> (Nothing, entries)
      Left msg -> (Just msg, entries)
 where
  defaultState =
    TypeCheckState
      { _tcStack = []
      , _tcLocals = computeLocals mn isStatic
      , _tcNexts = []
      }

  dfs
    :: (PrimMonad m)
    => V.MVector (PrimState m) TypeCheckState
    -> IS.IntSet
    -> ExceptT (B.ByteCodeIndex, TypeCheckError) m ()
  dfs entries is = case IS.minView is of
    Nothing -> return ()
    Just (i, indicies) -> do
      let a = code ^?! codeByteCode . ix i . to B.opcode

      -- setExceptionState code entries i
      s1 <- VM.read entries i
      s2 <-
        withExceptT (i,) $
          execStateT (runReaderT (typecheck a) hry) (s1 & tcNexts .~ [i + 1])

      recalculate <- forM (s2 ^. tcNexts) (unifyOnto entries s2)

      exceptions <-
        forM [h | h <- code ^. codeExceptionTable, h ^. ehStart == i] $
          \h ->
            let hstate =
                  s1
                    & tcStack
                      .~ case h ^. ehCatchType of
                        Just cn -> [asTypeInfo cn]
                        Nothing ->
                          [asTypeInfo ("java/lang/Throwable" :: ClassName)]
                    & tcNexts
                      .~ []
             in unifyOnto entries hstate (h ^. ehHandler)

      dfs
        entries
        ( IS.fromList [i' | (i', b) <- recalculate ++ exceptions, b]
            <> indicies
        )

  unifyOnto entries s2 i = do
    prevState <- VM.read entries i
    case unifyState prevState s2 hry of
      Just meetState -> do
        VM.write entries i meetState
        return (i, prevState /= meetState)
      Nothing -> throwError (i, InconsistentStates i prevState s2)

-- typeCheckDebug ::
--   Hierarchy
--   -> AbsMethodId
--   -> Bool
--   -> Code
--   -> IO (Either (B.ByteCodeIndex, TypeCheckError) (V.Vector TypeCheckState))
-- typeCheckDebug hry mn isStatic code = mapM V.freeze =<< runExceptT go where
--   defaultState =
--     TypeCheckState
--     { _tcStack = [], _tcLocals = computeLocals mn isStatic, _tcNexts = []}

--   go ::
--     ExceptT (B.ByteCodeIndex, TypeCheckError)
--     IO (V.MVector (PrimState IO) TypeCheckState)
--   go = do
--     entries <- VM.replicate (V.length $ code ^.codeByteCode) defaultState
--     iforMOf_ (codeByteCode.folded.to B.opcode) code $ \i a -> withExceptT (i,) $ do
--       setExceptionState code entries i
--       s1 <- VM.read entries i
--       debugInfo s1 i a
--       s2 <- execStateT (runReaderT (typecheck a) hry) (s1 & tcNexts .~ [i + 1])
--       runReaderT (updateStates entries s2) hry

--     return entries

debugInfo :: Int -> Code -> V.Vector TypeCheckState -> IO ()
debugInfo i (preview (codeByteCode . ix i) -> Just x) (preview (ix i) -> Just st) =
  liftIO $ do
    putStrLn ""
    putStrLn "Locals:"
    iforM_ (st ^. tcLocals) $
      \idx s -> putStrLn ("  " <> show idx <> ": " <> show s)
    putStrLn "Stack:"
    forM_ (reverse $ zip [0 ..] (st ^. tcStack)) $
      \(idx :: Int, s) -> putStrLn ("  " <> show idx <> ": " <> show s)
    putStrLn ""
    putStrLn $
      "BC:"
        <> show (B.offset x)
        <> " IX:"
        <> show i
        <> " - "
        <> show
          (B.opcode x)
debugInfo i _ _ = error $ "Could not find " <> show i

-- setExceptionState ::
--   PrimMonad m
--   => Code
--   -> V.MVector (PrimState m) TypeCheckState
--   -> B.ByteCodeIndex
--   -> m ()
-- setExceptionState code entries i =
--   forMOf_ (codeExceptionTable.folded.filtered (\h -> h^.ehHandler == i)) code $ \h -> do
--     hstate <- VM.read entries (h^.ehStart)
--     VM.write entries i $ hstate
--       & tcStack .~
--       case h^.ehCatchType of
--         Just cn -> [ asTypeInfo cn ]
--         Nothing -> [ asTypeInfo ("java/lang/Throwable" :: ClassName) ]
--       & tcLocals .~ hstate^.tcLocals

-- updateStates ::
--   (MonadReader Hierarchy m, MonadError TypeCheckError m, PrimMonad m)
--   => V.MVector (PrimState m) TypeCheckState
--   -> TypeCheckState
--   -> m ()
-- updateStates entries _state =
--   forM_ (_state^.tcNexts) $ \i -> do
--     prevState <- VM.read entries i
--     ask >>= \hry -> case (unifyState prevState _state hry) of
--       Just state' -> VM.write entries i state'
--       Nothing     -> throwError (InconsistentStates i prevState _state)

unifyState
  :: TypeCheckState -> TypeCheckState -> Hierarchy -> Maybe TypeCheckState
unifyState stk1 stk2 _ = do
  _tcStack <- (unify `on` view tcStack) stk1 stk2
  _tcLocals <- Just $ (IntMap.unionWith unify' `on` view tcLocals) stk2 stk1
  _tcNexts <- Just $ (List.union `on` view tcNexts) stk1 stk2
  return $ TypeCheckState{..}
 where
  unify' a b = case meet a b of
    Just a' -> a'
    Nothing -> TTop

  unify (a : s1) (b : s2) = do
    x <- meet a b
    (x :) <$> unify s1 s2
  unify [] s2 = return s2
  unify _ _ = Nothing

meet :: TypeInfo -> TypeInfo -> Maybe TypeInfo
meet = curry $ \case
  (TBase a, TBase b)
    | a == b -> Just (TBase a)
    | otherwise -> Nothing
  (TRef as, TRef bs) -> Just . TRef $ List.nub (as ++ bs)
  (TTop, a) -> Just a
  (a, TTop) -> Just a
  _ -> Nothing

computeLocals :: AbsMethodId -> Bool -> IntMap.IntMap TypeInfo
computeLocals mn isStatic = do
  IntMap.fromList $ zip (scanl (\n a -> n + typeSize a) 0 types) types
 where
  types =
    [asTypeInfo (mn ^. className) | not isStatic]
      ++ (mn ^.. methodIdArgumentTypes . folded . to asTypeInfo)

typeSize :: TypeInfo -> Int
typeSize = \case
  TBase TDouble -> 2
  TBase TLong -> 2
  _ -> 1

-- stack=1, locals=0, args_size=0
--   0: getstatic     #1  // Field $VALUES:[Lnet/dhleong/acl/enums/AlertStatus;
--   3: invokevirtual #2  // Method "[Lnet/dhleong/acl/enums/AlertStatus;".clone:()Ljava/lang/Object;
--   6: checkcast     #3  // class "[Lnet/dhleong/acl/enums/AlertStatus;"
--   9: areturn
-- LineNumberTable:
--  line 3: 0

-- | Get an element only if it is the only one.
_Null :: Prism' [a] ()
_Null =
  prism'
    (const [])
    ( \case
        (List.uncons -> Nothing) -> Just ()
        _ -> Nothing
    )

-- | Get an element only if it is the only one.
_Single :: Prism' [a] a
_Single =
  prism'
    (: [])
    ( \case
        (List.uncons -> Just (a, [])) -> Just a
        _ -> Nothing
    )

unexpected :: MonadError TypeCheckError m => String -> m a
unexpected = throwError . UnexpectedTypeError

-- | Given a single `Instruction` lets typecheck it.
typecheck
  :: forall m
   . ( MonadState TypeCheckState m
     , MonadReader Hierarchy m
     , MonadError TypeCheckError m
     )
  => Instruction
  -> m ()
typecheck = \case
  B.ArrayLoad r -> do
    pop >>= (`checkSubtypeOf` TInt)
    pop >>= isArray >>= \case
      TTop -> return ()
      a -> a `checkSubtypeOf` r
    push r
  B.ArrayStore r -> do
    b <- pop
    pop >>= (`checkSubtypeOf` TInt)
    at' <- isArray =<< pop

    case at' of
      TTop -> return ()
      a -> a `checkSubtypeOf` r

    b `checkSubtypeOf` at'
  B.Push r -> do
    push r
  B.Load r addr -> do
    lt <- getLocal addr
    lt `checkSubtypeOf` r
    push lt
  B.Store r addr -> do
    t <- pop
    t `checkSubtypeOf` r
    putLocal t addr
  B.BinaryOpr _ at' -> do
    pop >>= (`checkSubtypeOf` at')
    pop >>= (`checkSubtypeOf` at')
    push at'
  B.Neg r -> do
    a <- pop
    a `checkSubtypeOf` r
    push a
  B.BitOpr x s -> do
    let (bt', at') = case s of
          B.One -> (B.MInt, B.MInt)
          B.Two ->
            ( B.MLong
            , case x of
                B.ShL -> B.MInt
                B.ShR -> B.MInt
                B.UShR -> B.MInt
                _ -> B.MLong
            )
    pop >>= (`checkSubtypeOf` at')
    pop >>= (`checkSubtypeOf` bt')
    push bt'
  B.IncrLocal addr _ -> do
    getLocal addr >>= (`checkSubtypeOf` TInt)
  B.Cast cst -> case cst of
    B.CastDown _ -> do
      pop >>= (`checkSubtypeOf` TInt)
      push TInt
    B.CastTo a b -> do
      -- TODO: check for inequality
      pop >>= (`checkSubtypeOf` a)
      push b
  B.CompareLongs -> do
    pop >>= (`checkSubtypeOf` TLong)
    pop >>= (`checkSubtypeOf` TLong)
    push TInt
  B.CompareFloating _ size -> do
    case size of
      B.One -> do
        pop >>= (`checkSubtypeOf` TFloat)
        pop >>= (`checkSubtypeOf` TFloat)
      B.Two -> do
        pop >>= (`checkSubtypeOf` TDouble)
        pop >>= (`checkSubtypeOf` TDouble)
    push TInt
  B.IfZ a off -> do
    case a of
      B.CIs -> pop >>= (`checkSubtypeOf` TRef [])
      B.CIsNot -> pop >>= (`checkSubtypeOf` TRef [])
      _ -> pop >>= (`checkSubtypeOf` TInt)
    addNext off
  B.If a off -> do
    case a of
      B.CIs -> do
        pop >>= (`checkSubtypeOf` TRef [])
        pop >>= (`checkSubtypeOf` TRef [])
      B.CIsNot -> do
        pop >>= (`checkSubtypeOf` TRef [])
        pop >>= (`checkSubtypeOf` TRef [])
      _ -> do
        pop >>= (`checkSubtypeOf` TInt)
        pop >>= (`checkSubtypeOf` TInt)
    addNext off
  B.Goto off -> do
    setNext off
  B.Jsr off -> do
    push (TRef [])
    setNext off
  B.Ret off -> do
    void . unpack (_TRef . _Null) =<< getLocal off
    -- See https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-4.html#jvms-4.10.2.5
    -- TODO: Do cracy jre resolution?
    noNext
  B.TableSwitch def table -> do
    pop >>= (`checkSubtypeOf` TInt)
    setNext def
    mapM_ addNext (B.switchOffsets table)
  B.LookupSwitch def l -> do
    pop >>= (`checkSubtypeOf` TInt)
    setNext def
    mapM_ (addNext . snd) l
  B.Get fa fid -> do
    case fa of
      B.FldStatic -> return ()
      B.FldField -> do
        pop >>= (`checkSubtypeOf` fid ^. className)
    push (fid ^. fieldIdType)
  B.Put fa fid -> do
    pop >>= (`checkSubtypeOf` fid ^. fieldIdType)
    case fa of
      B.FldStatic -> return ()
      B.FldField -> do
        pop >>= (`checkSubtypeOf` fid ^. className)
  B.Invoke a -> do
    case a of
      B.InvkVirtual m -> do
        popArguments m
        pop >>= (`checkSubtypeOf` m ^. asInClass . className)
        pushReturn m
      B.InvkStatic (B.AbsVariableMethodId _ m) -> do
        popArguments m
        pushReturn m
      B.InvkSpecial (B.AbsVariableMethodId _ m) -> do
        popArguments m
        pop >>= (`checkSubtypeOf` m ^. asInClass . className)
        pushReturn m
      B.InvkInterface _ (B.AbsInterfaceMethodId m) -> do
        popArguments m
        pop >>= (`checkSubtypeOf` m ^. asInClass . className)
        pushReturn m
      B.InvkDynamic (B.InvokeDynamic _ m) -> do
        popArguments m
        pushReturn m
   where
    popArguments :: HasMethodId a => a -> m ()
    popArguments m = forM_ (reverse $ m ^. methodIdArgumentTypes) $ \b -> do
      pop >>= (`checkSubtypeOf` b)

    pushReturn :: HasMethodId a => a -> m ()
    pushReturn m = forMOf_ (methodIdReturnType . _Just) m push
  B.New a -> do
    push (B.JTClass a)
  B.NewArray a@(B.NewArrayType n _) -> do
    replicateM_ (fromIntegral n) (pop >>= (`checkSubtypeOf` TInt))
    push (B.newArrayTypeType a)
  B.ArrayLength -> do
    void . isArray =<< pop
    push TInt
  B.Throw -> do
    a <- pop
    a `checkSubtypeOf` B.JTClass "java/lang/Throwable"
    push a
    noNext
  B.InstanceOf _ -> do
    -- (trg `checkSubtypeOf`) =<< pop
    void . unpack _TRef =<< pop
    push TInt
  B.CheckCast trg -> do
    -- (trg `checkIntersectOf`) =<< pop
    void . unpack _TRef =<< pop
    push (B.JTRef trg)
  B.Monitor _ -> void . unpack _TRef =<< pop
  B.Return a -> do
    case a of
      Just a' -> pop >>= (`checkSubtypeOf` a')
      Nothing -> return ()
    noNext
  B.Nop -> return ()
  B.Pop size -> case size of
    B.One -> do
      a <- pop
      when (typeSize a == 2) $ unexpected "Trying to pop a two sized value"
    B.Two -> do
      a <- pop
      unless (typeSize a == 2) $ do
        b <- pop
        when (typeSize b == 2) $
          unexpected "Trying to pop a two sized value, as the second parameter"
  B.Dup size -> case size of
    B.One -> do
      a <- pop
      when (typeSize a == 2) $ unexpected "Trying to dup a two sized value"
      push a
      push a
    B.Two -> do
      a <- pop
      if typeSize a == 2
        then do
          push a
          push a
        else do
          b <- pop
          push b
          push a
          push b
          push a
  B.DupX1 size -> case size of
    B.One -> do
      a <- pop
      when (typeSize a == 2) $ unexpected "Trying to dupX1 a two sized value"
      b <- pop
      when (typeSize b == 2) $ unexpected "Trying to skip a two sized value (dupX1)"
      push a
      push b
      push a
    B.Two -> do
      a <- pop
      if typeSize a == 2
        then do
          b <- pop
          when (typeSize b == 2) $
            unexpected "Trying to skip a two sized value (dupX1)"
          push a
          push b
        else do
          b <- pop
          when (typeSize b == 2) $
            unexpected
              "Trying to dupX1 a two sized value, as the second parameter"
          c <- pop
          when (typeSize c == 2) $
            unexpected "Trying to skip a two sized value (dupX1)"
          push b
          push a
          push c
          push b
      push a
  B.DupX2 size -> case size of
    B.One -> do
      a <- pop
      when (typeSize a == 2) $ unexpected "Trying to dupX2 a two sized value"
      b <- pop
      if typeSize b == 2
        then do
          push a
          push b
          push a
        else do
          c <- pop
          when (typeSize c == 2) $
            unexpected
              "Trying to skip a two sized value (dupX2), as the second parameter"
          push a
          push c
          push b
          push a
    B.Two -> do
      a <- pop
      if typeSize a == 2
        then do
          b <- pop
          if typeSize b == 2
            then do
              push a
              push b
              push a
            else do
              c <- pop
              when (typeSize c == 2) $
                unexpected "Trying to skip a two sized value (dupX2)"
              push a
              push c
              push b
              push a
        else do
          b <- pop
          when (typeSize b == 2) $
            unexpected
              "Trying to copy a two sized value (dupX2) as the second parameter"
          c <- pop
          if typeSize c == 2
            then do
              push b
              push a
              push c
              push b
              push a
            else do
              d <- pop
              when (typeSize d == 2) $
                unexpected "Trying to skip a two sized value (dupX2)"
              push b
              push a
              push d
              push c
              push b
              push a
  B.Swap -> do
    a <- pop
    when (typeSize a == 2) $ unexpected "Trying to swap a two sized value"
    b <- pop
    when (typeSize b == 2) $ unexpected "Trying to swap a two sized value"
    push b
    push a

$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 3} ''TBase)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 3} ''TypeInfo)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 3} ''TypeCheckState)
$(deriveJSON defaultOptions{fieldLabelModifier = camelTo2 '_' . drop 3} ''TypeCheckError)
