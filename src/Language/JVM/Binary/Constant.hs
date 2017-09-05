{-# LANGUAGE RankNTypes #-}
module Language.JVM.Binary.Constant
  ( Constant (..)
  , ConstantRef (..)
  , ConstantPool (..)
  , poolSize

  , lookup
  , lookupL
  , toText
  , lookupText
  , toClassName
  , lookupClassName
  , lookupFieldDescriptor
  , lookupMethodDescriptor

  , Type.FieldDescriptor (..)
  , Type.MethodDescriptor (..)
  ) where

import           Prelude                  hiding (lookup, fail)

import           Control.Monad            (forM_)
import           Control.Monad.Fail        (fail)

import qualified Data.ByteString          as BS
import qualified Data.IntMap.Strict       as IM
import           Data.Word

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put


import Control.Lens

import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as TE

import qualified Language.JVM.Type as Type

import           Language.JVM.ClassName (ClassName(ClassName))

newtype ConstantRef =
  ConstantRef Word16
  deriving (Eq, Show)

instance Binary ConstantRef where
  get = ConstantRef <$> get
  put (ConstantRef x) = put x

data Constant
  = String BS.ByteString
  | Integer !Word32
  | Float !Word32
  | Long !Word64
  | Double !Word64
  | ClassRef !ConstantRef
  | StringRef !ConstantRef
  | FieldRef ConstantRef ConstantRef
  | MethodRef ConstantRef ConstantRef
  | InterfaceMethodRef ConstantRef ConstantRef
  | NameAndType ConstantRef ConstantRef
  | MethodHandle Word8 ConstantRef
  | MethodType ConstantRef
  | InvokeDynamic ConstantRef ConstantRef
  deriving (Show, Eq)

instance Binary Constant where
  get = do
    ident <- getWord8
    case ident of
      1 -> do
        len <- getInt16be
        String <$> getByteString (fromIntegral len)
      3 -> Integer <$> getWord32be
      4 -> Float <$> getWord32be
      5 -> Long <$> getWord64be
      6 -> Double <$> getWord64be
      7 -> ClassRef <$> get
      8 -> StringRef <$> get
      9 -> FieldRef <$> get <*> get
      10 -> MethodRef <$> get <*> get
      11 -> InterfaceMethodRef <$> get <*> get
      12 -> NameAndType <$> get <*> get
      15 -> MethodHandle <$> getWord8 <*> get
      16 -> MethodType <$> get
      18 -> InvokeDynamic <$> get <*> get
      _ -> fail $ "Unkown identifier " ++ show ident

  put x =
    case x of
      String bs -> do
        putWord8 1
        putInt16be . fromIntegral $ BS.length bs
        putByteString bs
      Integer i -> do
        putWord8 3
        putWord32be i
      Float i -> do
        putWord8 4
        putWord32be i
      Long i -> do
        putWord8 5
        putWord64be i
      Double i -> do
        putWord8 6
        putWord64be i
      ClassRef i -> do
        putWord8 7
        put i
      StringRef i -> do
        putWord8 8
        put i
      FieldRef i j -> do
        putWord8 9
        put i
        put j
      MethodRef i j -> do
        putWord8 10
        put i
        put j
      InterfaceMethodRef i j -> do
        putWord8 11
        put i
        put j
      NameAndType i j -> do
        putWord8 12
        put i
        put j
      MethodHandle i j -> do
        putWord8 15
        putWord8 i
        put j
      MethodType i -> do
        putWord8 16
        put i
      InvokeDynamic i j -> do
        putWord8 18
        put i
        put j

newtype ConstantPool = ConstantPool
  { unConstantPool :: IM.IntMap Constant
  } deriving (Show, Eq)

poolSize :: Constant -> Int
poolSize x =
  case x of
    Double _ ->  2
    Long _   -> 2
    _        -> 1

instance Binary ConstantPool where
  get = do
    len <- fromIntegral <$> getInt16be
    list <- go len 1
    return . ConstantPool $ IM.fromList list
    where
      go len n | len > n = do
        constant <- get
        rest <- go len (n + poolSize constant)
        return $ (n, constant) : rest
      go _ _ = return []
  put (ConstantPool p)= do
    case IM.maxViewWithKey p of
      Just ((key, e), _) -> do
        putInt16be (fromIntegral (key + poolSize e))
        forM_ (IM.toAscList p) (put . snd)
      Nothing -> do
        putInt16be 0

lookupL :: ConstantPool -> Getter ConstantRef (Maybe Constant)
lookupL = to . flip lookup

lookup :: ConstantRef -> ConstantPool -> Maybe Constant
lookup (ConstantRef ref) (ConstantPool cp) =
  IM.lookup (fromIntegral ref) cp

toText :: ConstantPool -> Getter ConstantRef (Maybe Text.Text)
toText = to . flip lookupText

lookupText :: ConstantRef -> ConstantPool -> Maybe Text.Text
lookupText ref cp = do
  String str <- lookup ref cp
  case TE.decodeUtf8' str of
    Left _  -> Nothing
    Right txt -> Just txt

toClassName :: ConstantPool -> Getter ConstantRef (Maybe ClassName)
toClassName = to . flip lookupClassName

lookupClassName :: ConstantRef -> ConstantPool -> Maybe ClassName
lookupClassName ref cp = do
  (ClassRef r) <- lookup ref cp
  ClassName <$> lookupText r cp

lookupFieldDescriptor :: ConstantRef -> ConstantPool -> Maybe Type.FieldDescriptor
lookupFieldDescriptor ref cp = do
  txt <- lookupText ref cp
  case Type.fieldDescriptorFromText txt of
    Right e -> return e
    Left f  -> fail f

lookupMethodDescriptor :: ConstantRef -> ConstantPool -> Maybe Type.MethodDescriptor
lookupMethodDescriptor ref cp = do
  txt <- lookupText ref cp
  case Type.methodDescriptorFromText txt of
    Right e -> return e
    Left f  -> fail f
