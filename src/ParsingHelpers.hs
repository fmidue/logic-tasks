{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module ParsingHelpers (
  Parser,
  formulaSymbol,
  whitespace,
  lexeme,
  parens,
  brackets,
  token,
  keyword,
  tokenSymbol,
  fully,
  infixl1,
  infixr1,
  prefix,
  (<$$>),
  ) where

import Control.Applicative ((<**>))

import Data.Char (isLetter)
import Data.Functor (void)
import Data.List.Extra (nubOrd)
import Data.Void

import Text.Megaparsec (Parsec, (<|>), try, notFollowedBy, eof, oneOf, satisfy)
import Text.Megaparsec.Char (space, alphaNumChar, string)

import Trees.Types (showOperator, showOperatorNot, allBinaryOperators)

type Parser = Parsec Void String

formulaSymbol :: Parser Char
formulaSymbol = satisfy isLetter
    <|> oneOf (nubOrd ("()" ++ showOperatorNot ++ concatMap showOperator allBinaryOperators))

whitespace :: Parser ()
whitespace = space

parens :: Parser a -> Parser a
parens p = try $ tokenSymbol "(" *> p <* tokenSymbol ")"

{-# Deprecated brackets "Use parens instead" #-}
brackets :: Parser a -> Parser a
brackets = parens

lexeme :: Parser a ->  Parser a
lexeme x = x <* space

token :: Parser a -> Parser a
token = lexeme . try

keyword :: String -> Parser ()
keyword k = token (string k *> notFollowedBy alphaNumChar)

tokenSymbol :: String -> Parser ()
tokenSymbol = token . void . string

fully :: Parser a -> Parser a
fully p = space *> p <* eof

infixl1 ::(a -> b) -> Parser a -> Parser (b -> a -> b) -> Parser b
infixl1 wrap p op = wrap <$> p <**> rest where
  -- rest :: Parser (b -> b)
  rest = flip (.) <$> (flip <$> op <*> p) <*> rest <|> pure id

infixr1 :: (a -> b) -> Parser a -> Parser (a -> b -> b) -> Parser b
infixr1 wrap p op = p <**> (flip <$> op <*> infixr1 wrap p op <|> pure wrap)

prefix :: (a -> b) -> Parser (b -> b) -> Parser a -> Parser b
prefix wrap op p = op <*> prefix wrap op p <|> wrap <$> p

(<$$>) :: Applicative f => f a -> (a -> b) -> f b
x <$$> f = f <$> x
