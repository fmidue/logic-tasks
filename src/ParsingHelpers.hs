{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module ParsingHelpers (
  formulaSymbol,
  whitespace,
  lexeme,
  ) where

import Text.Parsec.Char (oneOf, satisfy, spaces)
import Data.Char (isLetter)
import Text.Parsec ((<|>))
import Text.Parsec.String (Parser)
import Trees.Types (showOperator, showOperatorNot, allBinaryOperators)
import Data.List.Extra (nubOrd)

formulaSymbol :: Parser Char
formulaSymbol = satisfy isLetter
    <|> oneOf (nubOrd ("()" ++ showOperatorNot ++ concatMap showOperator allBinaryOperators))

whitespace :: Parser ()
whitespace = spaces

lexeme :: Parser a -> Parser a
lexeme p = do
    x <- p
    whitespace
    return x
