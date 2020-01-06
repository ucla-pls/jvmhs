{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
module Jvmhs.Format.Internal where


import           Prelude                 hiding ( id
                                                , (.)
                                                )
import           Control.Category
import qualified Data.List                     as List
import           Data.Maybe
import           Data.Coerce
import           Control.Monad

-- hashable
import           Data.Hashable

-- unordered-containers
import qualified Data.HashMap.Strict           as HashMap

import           Control.Lens


-- | Glass is a partial isomorphism it might be partial in both direction.
type Glass s t a b
  = forall p f . (Profunctor p, Functor f) => p a (f b) -> p s (f t)

type Glass' s a = Glass s s a a

type AnGlass s t a b = Parts a b a (Identity b) -> Parts a b s (Identity t)

data Parts a b s t =
  Parts (s -> Validation [String] a) (b -> Validation [String] t)

instance Profunctor (Parts a b) where
  dimap f g (Parts sa bt) = Parts (sa . f) (fmap g . bt)
  {-# INLINE dimap #-}
  lmap f (Parts sa bt) = Parts (sa . f) bt
  {-# INLINE lmap #-}
  rmap f (Parts sa bt) = Parts sa (fmap f . bt)
  {-# INLINE rmap #-}


-- | Validation is an Either which can collect it's faliures. This
-- is usefull when presenting this information to the user later.
data Validation e a
  = Success !a
  | Failure !e
  deriving (Eq, Ord, Show, Functor)

-- | An either can be turned into a validation
validateEither :: Either e a -> Validation [e] a
validateEither = \case
  Left  e -> Failure [e]
  Right a -> Success a

instance Semigroup e => Applicative (Validation e) where
  pure = Success
  {-# INLINE pure #-}
  fa <*> a = case fa of
    Success fn -> fn <$> a
    Failure e  -> case a of
      Success _  -> Failure e
      Failure e' -> Failure (e <> e')
  {-# INLINE (<*>) #-}

instance Semigroup e => Monad (Validation e) where
  return = pure
  {-# INLINE return #-}
  ma >>= fma = case ma of
    Success a -> fma a
    Failure e -> Failure e
  {-# INLINE (>>=) #-}

-- | A Partial Isomorphism, we use them here allow convertion between the
-- two formats, while still perserving bugs. Every partial isomorphism
-- should be a valid adjunction.
-- This means that
-- >> there p . back p . there p == there p
data PartIso e a b = PartIso
  { there :: a -> Validation e b
  , back  :: b -> Validation e a
  }

instance Semigroup e => Category (PartIso e) where
  id = PartIso Success Success
  {-# INLINE id #-}
  (.) (PartIso f1 t1) (PartIso f2 t2) = PartIso (f1 <=< f2) (t1 >=> t2)
  {-# INLINE (.) #-}

infixr 3 ***
(***)
  :: Semigroup e => PartIso e a c -> PartIso e b d -> PartIso e (a, b) (c, d)
(***) (PartIso f1 t1) (PartIso f2 t2) =
  PartIso (\(a, b) -> (,) <$> f1 a <*> f2 b) (\(c, d) -> (,) <$> t1 c <*> t2 d)
{-# INLINE (***) #-}

inSecond :: Semigroup e => PartIso e b d -> PartIso e (c, b) (c, d)
inSecond f = id *** f
{-# INLINE inSecond #-}

inFirst :: Semigroup e => PartIso e a c -> PartIso e (a, b) (c, b)
inFirst f = f *** id
{-# INLINE inFirst #-}

triple
  :: Semigroup e
  => PartIso e a a'
  -> PartIso e b b'
  -> PartIso e c c'
  -> PartIso e (a, b, c) (a', b', c')
triple (PartIso f1 t1) (PartIso f2 t2) (PartIso f3 t3) = PartIso
  (\(a, b, c) -> (,,) <$> f1 a <*> f2 b <*> f3 c)
  (\(a, b, c) -> (,,) <$> t1 a <*> t2 b <*> t3 c)

quadruple
  :: Semigroup e
  => PartIso e a a'
  -> PartIso e b b'
  -> PartIso e c c'
  -> PartIso e d d'
  -> PartIso e (a, b, c, d) (a', b', c', d')
quadruple (PartIso f1 t1) (PartIso f2 t2) (PartIso f3 t3) (PartIso f4 t4) =
  PartIso (\(a, b, c, d) -> (,,,) <$> f1 a <*> f2 b <*> f3 c <*> f4 d)
          (\(a, b, c, d) -> (,,,) <$> t1 a <*> t2 b <*> t3 c <*> t4 d)

quintuple
  :: Semigroup e
  => PartIso e a a'
  -> PartIso e b b'
  -> PartIso e c c'
  -> PartIso e d d'
  -> PartIso e f f'
  -> PartIso e (a, b, c, d, f) (a', b', c', d', f')
quintuple (PartIso f1 t1) (PartIso f2 t2) (PartIso f3 t3) (PartIso f4 t4) (PartIso f5 t5)
  = PartIso
    (\(a, b, c, d, f) -> (,,,,) <$> f1 a <*> f2 b <*> f3 c <*> f4 d <*> f5 f)
    (\(a, b, c, d, f) -> (,,,,) <$> t1 a <*> t2 b <*> t3 c <*> t4 d <*> t5 f)



introItem :: Eq x => x -> PartIso [String] () x
introItem x = PartIso
  (\() -> Success x)
  (\x' -> if x' == x then Success () else Failure ["Const item is not returned"]
  )
{-# INLINE introItem #-}

introFirst :: Semigroup e => PartIso e a ((), a)
introFirst = fromIso (\a -> ((), a)) (\((), a) -> a)
{-# INLINE introFirst #-}

introSecond :: Semigroup e => PartIso e a (a, ())
introSecond = fromIso (\a -> (a, ())) (\(a, ()) -> a)
{-# INLINE introSecond #-}

constFirst :: Eq x => x -> PartIso [String] a (x, a)
constFirst x = inFirst (introItem x) . introFirst
{-# INLINE constFirst #-}

constSecond :: Eq x => x -> PartIso [String] a (a, x)
constSecond x = inSecond (introItem x) . introSecond
{-# INLINE constSecond #-}


-- | Change the direction of the partial isomorphism
flipDirection :: PartIso e a b -> PartIso e b a
flipDirection (PartIso t b) = PartIso b t

-- | Create a partial isomorphism from an isomorphism
fromIso :: (a -> b) -> (b -> a) -> PartIso e a b
fromIso f t = PartIso (Success . f) (Success . t)

-- | Create a partial isomorphism from an isomorphism
fromPrism :: Prism' a b -> PartIso e (Maybe a) (Maybe b)
fromPrism _P = fromIso
  (\case
    Nothing -> Nothing
    Just a  -> preview _P a
  )
  (\case
    Nothing -> Nothing
    Just a  -> Just (review _P a)
  )

-- | Create a partial isomorphism from an isomorphism
fromPrism' :: (a -> Validation e b) -> Prism' a b -> PartIso e a b
fromPrism' err _P =
  PartIso (\a -> maybe (err a) Success . preview _P $ a) (Success . review _P)

fromLens :: Lens s t a b -> Lens t s b a -> PartIso e a b -> PartIso e s t
fromLens lns snl (PartIso afb bfa) = PartIso (lns afb) (snl bfa)

-- | Anything that can be coerced is an isomorphism.
coerceFormat :: (Coercible a b, Coercible b a) => PartIso e a b
coerceFormat = fromIso coerce coerce

-- | Anything that can be coerced is an isomorphism.
isomap :: (Traversable t, Semigroup e) => PartIso e a b -> PartIso e (t a) (t b)
isomap (PartIso f t) = PartIso (traverse f) (traverse t)



-- | Given an isomorphism from a to list of b's, then create 
-- a compression that create a 
compressList :: Semigroup e => PartIso e a [b] -> PartIso e [a] [b]
compressList (PartIso f t) = PartIso
  (fmap concat . mapM f)
  (\case
    [] -> pure []
    bs -> (: []) <$> t bs
  )

-- | If a list contains no more than one element then we can convert it 
-- into a maybe. This is not checked.
singletonList :: Semigroup e => PartIso e [a] (Maybe a)
singletonList = fromIso listToMaybe maybeToList

zipList :: Semigroup e => PartIso e ([a], [b]) [(a, b)]
zipList = fromIso (uncurry List.zip) (List.unzip)

withDefaultF :: PartIso e (a, Maybe a) a
withDefaultF = fromIso (\(a, ma) -> fromMaybe a ma) (\a -> (a, Just a))

partitionList :: (a -> Bool) -> PartIso e [a] ([a], [a])
partitionList p = fromIso (List.partition p) (uncurry (++))

mkHashMap :: (Eq a, Hashable a) => PartIso e [(a, b)] (HashMap.HashMap a b)
mkHashMap = fromIso HashMap.fromList HashMap.toList
