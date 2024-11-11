{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts #-}

module ParsingHelpers (
  formulaSymbol,
  whitespace,
  lexeme,
  parens,
  brackets,
  caseInsensitive,
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

import Data.Char (isLetter, toLower, toUpper)
import Data.Functor (void)
import Data.List.Extra (nubOrd)

import Text.Parsec ((<|>), try, notFollowedBy, alphaNum, string, eof)
import Text.Parsec.Char (char, oneOf, satisfy, spaces)
import Text.Parsec.String (Parser)

import Trees.Types (showOperator, showOperatorNot, allBinaryOperators)

formulaSymbol :: Parser Char
formulaSymbol = satisfy isLetter
    <|> oneOf (nubOrd ("()" ++ showOperatorNot ++ concatMap showOperator allBinaryOperators))

whitespace :: Parser ()
whitespace = spaces

parens :: Parser a -> Parser a
parens p = try $ tokenSymbol "(" *> p <* tokenSymbol ")"

{-# Deprecated brackets "Use parens instead" #-}
brackets :: Parser a -> Parser a
brackets = parens

caseInsensitive :: String -> Parser String
caseInsensitive = mapM caseInsensitiveChar
  where
    caseInsensitiveChar c = do
      char (toLower c) <|> char (toUpper c)
      return c

lexeme :: Parser a ->  Parser a
lexeme x = x <* spaces

token :: Parser a -> Parser a
token = lexeme . try

keyword :: String -> Parser ()
keyword k = token (string k *> notFollowedBy alphaNum)

tokenSymbol :: String -> Parser ()
tokenSymbol = token . void . string

fully :: Parser a -> Parser a
fully p = spaces *> p <* eof

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
