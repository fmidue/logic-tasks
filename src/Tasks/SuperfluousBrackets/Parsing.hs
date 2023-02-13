module Tasks.SuperfluousBrackets.Parsing (
  superfluousBracketsExcParser
  ) where

import Text.Parsec.String (Parser)
import Text.Parsec (eof, ParseError, parse, many1, (<|>))
import Text.Parsec.Char (satisfy, string)
import ParsingHelpers (whitespace, lexeme)

import Data.Char (isLetter)
import Trees.Types (showOperator, showOperatorNot, allBinaryOperators)

parseLetterToStr :: Parser String
parseLetterToStr = do
    letter <- satisfy isLetter
    return [letter]

operatorAndLeavesParse :: Parser String
operatorAndLeavesParse = do
    listOfString <- many1 . lexeme $
      parseLetterToStr <|> foldr
        ((<|>) . string . showOperator)
        (string "(" <|> string ")" <|> string showOperatorNot) allBinaryOperators
    return (concat listOfString)

superfluousBracketsExcParser :: String -> Either ParseError String
superfluousBracketsExcParser = parse (whitespace >> operatorAndLeavesParse <* eof) ""
