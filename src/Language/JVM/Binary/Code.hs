{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.JVM.Binary.Code
  ( Code (..)

  , maxStack
  , maxLocals
  , bytecode
  , exceptionTable
  , attributes

  , ByteCode (..)
  , ExceptionTable (..)

  , Constant (..)
  , WordSize
  , OneOrTwo (..)
  , ArithmeticType (..)
  , LocalType (..)
  , ArrayType (..)

  , ByteCodeInst (..)

  , fromAttribute
  ) where

import Prelude hiding (fail)
import qualified Prelude as P

import Numeric (showHex)

import Control.Monad.Fail (fail)
import Control.Monad hiding (fail)

import Control.Lens

import qualified Data.Vector as V

import Data.Binary
import Data.Binary.Get hiding (Get)
import Data.Binary.Put hiding (Put)

import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy as BL

import Data.Int

import Language.JVM.Binary.SizedList
import Language.JVM.Binary.Attribute

import Language.JVM.Binary.Constant (ConstantRef(..))

data Code = Code
  { _maxStack :: Int16
  , _maxLocals :: Int16
  , _bytecode :: ByteCode
  , _exceptionTable :: SizedList16 ExceptionTable
  , _attributes :: SizedList16 Attribute
  } deriving (Show, Eq, Generic)

instance Binary Code where
  -- Auto implemented by generic

newtype ByteCode = ByteCode
  { unByteCode :: [ByteCodeInst]
  } deriving (Show, Eq)

instance Binary ByteCode where
  get = do
    x <- getWord32be
    bs <- getLazyByteString (fromIntegral x)
    case runGetOrFail go bs of
      Right (_,_,bcs) -> return $ ByteCode bcs
      Left (_,_,msg) -> fail msg
    where
      go = isEmpty >>= \t -> if t then return [] else (:) <$> get <*> go


  put (ByteCode lst)= do
    let bs = runPut (mapM_ put lst)
    putWord32be (fromIntegral $ BL.length bs)
    putLazyByteString bs


data ExceptionTable = ExceptionTable
  { start :: Int32
  -- ^ Inclusive program counter into 'code'
  , end :: Int32
  -- ^ Exclusive program counter into 'code'
  , handler :: Int32
  -- ^ A program counter into 'code' indicating the code.
  , catchType :: ConstantRef
  }
  deriving (Show, Eq, Generic)

instance Binary ExceptionTable where

fromAttribute :: Attribute -> Either String Code
fromAttribute attr =
  case runGetOrFail get (BL.fromStrict $ attr ^. info) of
    Left (_,_,msg) -> Left msg
    Right (_,_,a) -> Right a


data ArithmeticType = MInt | MLong | MFloat | MDouble
  deriving (Show, Eq, Enum, Bounded)

data SmallArithmeticType = MByte | MChar | MShort
  deriving (Show, Eq, Enum, Bounded)

data LocalType = LInt | LLong | LFloat | LDouble | LRef
  deriving (Show, Eq, Enum, Bounded)

data ArrayType a
  = AByte | AChar | AShort | AInt | ALong
  | AFloat | ADouble | ARef a
  deriving (Show, Eq)

data Invokation
  = InvkSpecial
  | InvkVirtual
  | InvkStatic
  | InvkInterface Word8
  | InvkDynamic
  deriving (Show, Eq)

data FieldAccess
  = FldStatic
  | FldField
  deriving (Show, Eq)

data OneOrTwo = One | Two
  deriving (Show, Ord, Bounded, Eq, Enum)

type WordSize = OneOrTwo

data Constant
  = CNull

  | CIntM1
  | CInt0
  | CInt1
  | CInt2
  | CInt3
  | CInt4
  | CInt5

  | CLong0
  | CLong1

  | CFloat0
  | CFloat1
  | CFloat2

  | CDouble0
  | CDouble1

  | CByte Int8
  | CShort Int16

  | CHalfRef ConstantRef
  | CRef WordSize ConstantRef
  deriving (Show, Eq)

data BinOpr
  = Add
  | Sub
  | Mul
  | Div
  | Rem
  deriving (Show, Eq)

data BitOpr
  = ShL
  | ShR
  | UShR
  | And
  | Or
  | XOr
  deriving (Show, Eq)

type Offset = Int16
type LongOffset = Int32
type LocalAddress = Word8

data CmpOpr
  = CEq | CNe | CLt | CGe | CGt | CLe
  deriving (Show, Eq)

data ByteCodeInst
  = ArrayLoad (ArrayType ())
  -- ^ aaload baload ...
  | ArrayStore (ArrayType ())
  -- ^ aastore bastore ...

  | Push Constant

  | Load LocalType LocalAddress
  -- ^ aload_0, bload_2, iload 5 ...
  | Store LocalType LocalAddress
  -- ^ aload, bload ...

  | BinaryOpr BinOpr ArithmeticType
  -- ^ iadd ...
  | Neg ArithmeticType
  -- ^ ineg ...

  | BitOpr BitOpr WordSize
  -- ^ Exclusively on int and long, identified by the word-size

  | IncrLocal LocalAddress Int8
  -- ^ Only works on ints, increment local #1, with #2

  | Cast ArithmeticType ArithmeticType
  -- ^ Only valid on different types

  | CastDown SmallArithmeticType
  -- ^ Only valid on different types

  | CompareLongs

  | CompareFloating Bool WordSize
  -- ^ Compare two floating values, #1 indicates if greater-than, #2
  -- is if float or double should be used.

  | If CmpOpr OneOrTwo Offset
  -- ^ compare with 0 if #2 is False, and two ints from the stack if
  -- True. the last value is the offset

  | IfRef Bool OneOrTwo Offset
  -- ^ check if two objects are equal, or not equal. If #2 is True, compare
  -- with null.

  | Goto LongOffset
  | Jsr LongOffset
  | Ret LocalAddress

  | TableSwitch Int32 Int32 Int32 (V.Vector Int32)
  -- ^ a table switch has 3 values `default` `low` `high` and a list of
  -- offets.
  | LookupSwitch Int32 (V.Vector (Int32, Int32))
  -- ^ a lookup switch has a `default` value and a list of pairs.

  | Get FieldAccess ConstantRef
  | Put FieldAccess ConstantRef

  | Invoke Invokation ConstantRef

  | New ConstantRef
  | NewArray (ArrayType ConstantRef)

  | ArrayLength

  | Throw

  | CheckCast ConstantRef
  | InstanceOf ConstantRef

  | Monitor Bool
  -- ^ True => Enter, False => Exit

  | MultiNewArray ConstantRef Word8
  -- ^ Create a new multi array of #1 and with #2 dimensions

  | Return (Maybe LocalType)

  | Nop

  | Pop WordSize

  | Dup WordSize
  | DupX1 WordSize
  | DupX2 WordSize

  | Swap

  deriving (Show, Eq)

instance Binary ByteCodeInst where
  get = do
    cmd <- getWord8
    case cmd of
      0x00 -> return Nop
      0x01 -> return $ Push CNull

      0x02 -> return $ Push CIntM1
      0x03 -> return $ Push CInt0
      0x04 -> return $ Push CInt1
      0x05 -> return $ Push CInt2
      0x06 -> return $ Push CInt3
      0x07 -> return $ Push CInt4
      0x08 -> return $ Push CInt5

      0x09 -> return $ Push CLong0
      0x0a -> return $ Push CLong1

      0x0b -> return $ Push CFloat0
      0x0c -> return $ Push CFloat1
      0x0d -> return $ Push CFloat2

      0x0e -> return $ Push CDouble0
      0x0f -> return $ Push CDouble1

      0x10 -> Push . CByte <$> get
      0x11 -> Push . CShort <$> get

      0x12 -> Push . CHalfRef . ConstantRef . fromIntegral <$> getWord8
      0x13 -> Push . CRef One <$> get
      0x14 -> Push . CRef Two <$> get

      0x15 -> Load LInt <$> get
      0x16 -> Load LLong <$> get
      0x17 -> Load LFloat <$> get
      0x18 -> Load LDouble <$> get
      0x19 -> Load LRef <$> get

      0x1a -> return $ Load LInt 0
      0x1b -> return $ Load LInt 1
      0x1c -> return $ Load LInt 2
      0x1d -> return $ Load LInt 3

      0x1e -> return $ Load LLong 0

      0x20 -> return $ Load LLong 2
      0x21 -> return $ Load LLong 3

      0x22 -> return $ Load LFloat 0
      0x23 -> return $ Load LFloat 1
      0x24 -> return $ Load LFloat 2
      0x25 -> return $ Load LFloat 3

      0x26 -> return $ Load LDouble 0
      0x27 -> return $ Load LDouble 1
      0x28 -> return $ Load LDouble 2
      0x29 -> return $ Load LDouble 3

      0x2a -> return $ Load LRef 0
      0x2b -> return $ Load LRef 1
      0x2c -> return $ Load LRef 2
      0x2d -> return $ Load LRef 3

      0x2e -> return $ ArrayLoad AInt
      0x2f -> return $ ArrayLoad ALong
      0x30 -> return $ ArrayLoad AFloat
      0x31 -> return $ ArrayLoad ADouble
      0x32 -> return $ ArrayLoad (ARef ())
      0x33 -> return $ ArrayLoad AByte
      0x34 -> return $ ArrayLoad AChar
      0x35 -> return $ ArrayLoad AShort

      0x36 -> Store LInt <$> get
      0x37 -> Store LLong <$> get
      0x38 -> Store LFloat <$> get
      0x39 -> Store LDouble <$> get
      0x3a -> Store LRef <$> get

      0x3b -> return $ Store LInt 0
      0x3c -> return $ Store LInt 1
      0x3d -> return $ Store LInt 2
      0x3e -> return $ Store LInt 3

      0x3f -> return $ Store LLong 0
      0x40 -> return $ Store LLong 1
      0x41 -> return $ Store LLong 2
      0x42 -> return $ Store LLong 3

      0x43 -> return $ Store LFloat 0
      0x44 -> return $ Store LFloat 1
      0x45 -> return $ Store LFloat 2
      0x46 -> return $ Store LFloat 3

      0x47 -> return $ Store LDouble 0
      0x48 -> return $ Store LDouble 1
      0x49 -> return $ Store LDouble 2
      0x4a -> return $ Store LDouble 3

      0x4b -> return $ Store LRef 0
      0x4c -> return $ Store LRef 1
      0x4d -> return $ Store LRef 2
      0x4e -> return $ Store LRef 3

      0x4f -> return $ ArrayStore AInt
      0x50 -> return $ ArrayStore ALong
      0x51 -> return $ ArrayStore AFloat
      0x52 -> return $ ArrayStore ADouble
      0x53 -> return $ ArrayStore (ARef ())
      0x54 -> return $ ArrayStore AByte
      0x55 -> return $ ArrayStore AChar
      0x56 -> return $ ArrayStore AShort

      0x57 -> return $ Pop One
      0x58 -> return $ Pop Two

      0x59 -> return $ Dup One
      0x5a -> return $ DupX1 One
      0x5b -> return $ DupX2 One

      0x5c -> return $ Dup Two
      0x5d -> return $ DupX1 Two
      0x5e -> return $ DupX2 Two

      0x5f -> return $ Swap

      0x60 -> return $ BinaryOpr Add MInt
      0x61 -> return $ BinaryOpr Add MLong
      0x62 -> return $ BinaryOpr Add MFloat
      0x63 -> return $ BinaryOpr Add MDouble

      0x64 -> return $ BinaryOpr Sub MInt
      0x65 -> return $ BinaryOpr Sub MLong
      0x66 -> return $ BinaryOpr Sub MFloat
      0x67 -> return $ BinaryOpr Sub MDouble

      0x68 -> return $ BinaryOpr Mul MInt
      0x69 -> return $ BinaryOpr Mul MLong
      0x6a -> return $ BinaryOpr Mul MFloat
      0x6b -> return $ BinaryOpr Mul MDouble

      0x6c -> return $ BinaryOpr Div MInt
      0x6d -> return $ BinaryOpr Div MLong
      0x6e -> return $ BinaryOpr Div MFloat
      0x6f -> return $ BinaryOpr Div MDouble

      0x70 -> return $ BinaryOpr Rem MInt
      0x71 -> return $ BinaryOpr Rem MLong
      0x72 -> return $ BinaryOpr Rem MFloat
      0x73 -> return $ BinaryOpr Rem MDouble

      0x74 -> return $ Neg MInt
      0x75 -> return $ Neg MLong
      0x76 -> return $ Neg MFloat
      0x77 -> return $ Neg MDouble

      0x78 -> return $ BitOpr ShL One
      0x79 -> return $ BitOpr ShL Two
      0x7a -> return $ BitOpr ShR One
      0x7b -> return $ BitOpr ShR Two

      0x7c -> return $ BitOpr UShR One
      0x7d -> return $ BitOpr UShR Two

      0x7e -> return $ BitOpr And One
      0x7f -> return $ BitOpr And Two
      0x80 -> return $ BitOpr Or One
      0x81 -> return $ BitOpr Or Two
      0x82 -> return $ BitOpr XOr One
      0x83 -> return $ BitOpr XOr Two

      0x84 -> IncrLocal <$> get <*> get

      0x85 -> return $ Cast MInt MLong
      0x86 -> return $ Cast MInt MFloat
      0x87 -> return $ Cast MInt MDouble

      0x88 -> return $ Cast MLong MInt
      0x89 -> return $ Cast MLong MFloat
      0x8a -> return $ Cast MLong MDouble

      0x8b -> return $ Cast MFloat MInt
      0x8c -> return $ Cast MFloat MLong
      0x8d -> return $ Cast MFloat MDouble

      0x8e -> return $ Cast MDouble MInt
      0x8f -> return $ Cast MDouble MLong
      0x90 -> return $ Cast MDouble MFloat

      0x91 -> return $ CastDown MByte
      0x92 -> return $ CastDown MChar
      0x93 -> return $ CastDown MShort

      0x94 -> return $ CompareLongs

      0x95 -> return $ CompareFloating True One
      0x96 -> return $ CompareFloating False Two

      0x97 -> return $ CompareFloating True One
      0x98 -> return $ CompareFloating False Two

      0x99 -> If CEq One <$> get
      0x9a -> If CNe One <$> get
      0x9b -> If CLt One <$> get
      0x9c -> If CGe One <$> get
      0x9d -> If CGt One <$> get
      0x9e -> If CLe One <$> get

      0x9f -> If CEq Two <$> get
      0xa0 -> If CNe Two <$> get
      0xa1 -> If CLt Two <$> get
      0xa2 -> If CGe Two <$> get
      0xa3 -> If CGt Two <$> get
      0xa4 -> If CLe Two <$> get

      0xa5 -> IfRef True Two <$> get
      0xa6 -> IfRef False Two <$> get

      0xa7 -> Goto . fromIntegral <$> getInt16be
      0xa8 -> Jsr . fromIntegral <$> getInt16be
      0xa9 -> Ret <$> get

      0xaa -> do
        offset <- bytesRead
        skip . fromIntegral $ offset `mod` 4
        dft <- getInt32be
        low <- getInt32be
        high <- getInt32be
        table <- V.replicateM (fromIntegral $ high - low + 1) getInt32be
        return $ TableSwitch dft low high table

      0xab -> do
        offset <- bytesRead
        skip . fromIntegral $ offset `mod` 4
        dft <- getInt32be
        npairs <- getInt32be
        pairs <- V.replicateM (fromIntegral npairs) get
        return $ LookupSwitch dft pairs

      0xac -> return . Return . Just $ LInt
      0xad -> return . Return . Just $ LLong
      0xae -> return . Return . Just $ LFloat
      0xaf -> return . Return . Just $ LDouble
      0xb0 -> return . Return . Just $ LRef
      0xb1 -> return . Return $ Nothing

      0xb2 -> Get FldStatic <$> get
      0xb3 -> Put FldStatic <$> get

      0xb4 -> Get FldField <$> get
      0xb5 -> Put FldField <$> get

      0xb6 -> Invoke InvkVirtual <$> get
      0xb7 -> Invoke InvkSpecial <$> get
      0xb8 -> Invoke InvkStatic <$> get
      0xb9 -> do
        ref <- get
        count <- get
        when (count == 0) $ fail "Should be not zero"
        zero <- getWord8
        when (zero /= 0) $ fail "Should be zero"
        return $ Invoke (InvkInterface count) ref
      0xba -> do
        ref <- get
        count <- getWord8
        when (count /= 0) $ fail "Should be zero"
        zero <- getWord8
        when (zero /= 0) $ fail "Should be zero"
        return $ Invoke InvkDynamic ref
      0xbb -> New <$> get

      0xbc -> do
        x <- getWord8
        NewArray <$> case x of
          4 -> fail "Boolean arrays not supported... Sorry?"
          5 -> return AChar
          6 -> return AFloat
          7 -> return ADouble
          8 -> return AByte
          9 -> return AShort
          10 -> return AInt
          11 -> return ALong
          _ -> fail $ "Unknown type '0x" ++ showHex x "'."

      0xbd -> NewArray . ARef <$> get

      0xbe -> return ArrayLength

      0xbf -> return Throw

      0xc0 -> CheckCast <$> get
      0xc1 -> InstanceOf <$> get

      0xc2 -> return $ Monitor True
      0xc3 -> return $ Monitor False

      0xc4 -> fail "Wide not implemented yet"

      0xc5 -> MultiNewArray <$> get <*> get

      0xc6 -> IfRef False One <$> get
      0xc7 -> IfRef True One <$> get

      0xc8 -> Goto <$> getInt32be
      0xc9 -> Jsr <$> getInt32be

      _ -> fail $ "I do not know this bytecode '0x" ++ showHex cmd "'."


  put bc =
    case bc of
      Nop -> putInt8 0x00
      Push CNull -> putInt8 0x01

      Push CIntM1 -> putInt8 0x02
      Push CInt0 -> putInt8 0x03
      Push CInt1 -> putInt8 0x04
      Push CInt2 -> putInt8 0x05
      Push CInt3 -> putInt8 0x06
      Push CInt4 -> putInt8 0x07
      Push CInt5 -> putInt8 0x08

      Push CLong0 -> putInt8 0x09
      Push CLong1 -> putInt8 0x0a

      Push CFloat0 -> putInt8 0x0b
      Push CFloat1 -> putInt8 0x0c
      Push CFloat2 -> putInt8 0x0d

      Push CDouble0 -> putInt8 0x0e
      Push CDouble1 -> putInt8 0x0f

      Push (CByte x) -> putInt8 0x10 >> put x
      Push (CShort x) -> putInt8 0x11 >> put x

      Push (CHalfRef (ConstantRef r)) -> putInt8 0x12 >> putWord8 (fromIntegral r)
      Push (CRef One (ConstantRef r)) -> putInt8 0x13 >> put r
      Push (CRef Two (ConstantRef r)) -> putInt8 0x14 >> put r

      _ -> P.fail $ "Is not able to print '" ++ show bc ++ "' yet."

makeLenses ''Code
