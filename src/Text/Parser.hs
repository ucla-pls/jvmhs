{-# LANGUAGE DeriveFunctor #-}
module Text.Parser
   ( Parser (..)
   , parseAll
   , readChar
   , next
   , readSome
   , readWhile
   , readStar
   , (<|>)
   , fail
   ) where


import Prelude hiding (fail)

import qualified Data.Text as Text

import Control.Monad.Fail
import Control.Applicative

newtype Parser a =
  Parser { runParser :: Text.Text -> Either String (Text.Text, a) }
  deriving (Functor)

instance Applicative Parser where
  pure a =
    Parser (\text -> Right (text, a))

  (<*>) (Parser f) (Parser a) = Parser $ \text -> do
    (r1, f') <- f text
    (r2, a') <- a r1
    return (r2, f' a')

instance Monad Parser where
  return = pure

  (>>=) (Parser m1) f = Parser $ \text ->
    case m1 text of
      Right (rest, a) ->
        runParser (f a) rest
      Left err -> Left err

instance MonadFail Parser where
  fail msg = Parser (\_ -> Left msg)

instance Alternative Parser where
  empty = Control.Monad.Fail.fail "empty"

  (<|>) m1 m2 = Parser $ \text ->
    case runParser m1 text of
      Left _ -> runParser m2 text
      x -> x

parseAll :: Parser a -> Text.Text -> Either String a
parseAll p text =
  case runParser p text of
    Right (rest, fd) ->
      if Text.length rest /= 0 then
        Left $ "Unread text " ++ show rest
      else return fd
    Left msg -> Left msg


readChar :: Parser Char
readChar = Parser $ \text ->
  case Text.uncons text of
    Just (char, text') ->
      Right (text', char)
    Nothing ->
      Left "text is empty"

next :: Char -> Parser ()
next c = do
  char <- readChar
  if char == c
    then return ()
    else fail $ "Expected " ++ show c ++ " but got " ++ show char

readSome :: Int -> Parser Text.Text
readSome n = Parser $ \text ->
  let (first, rest) = Text.splitAt n text
  in if Text.length first /= n then
    Right (rest, first) else
    Left "Not enough characters left"

readWhile :: (Char -> Bool) -> Parser Text.Text
readWhile f = Parser $ \text ->
  let (e, rest) = Text.span f text
  in Right (rest, e)

readStar :: Parser a -> Parser [a]
readStar p =
  do { a <- p; as <- readStar p; return $ a : as } <|> return []

