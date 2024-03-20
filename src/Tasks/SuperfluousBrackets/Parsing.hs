
module Tasks.SuperfluousBrackets.Parsing (
    superfluousBracketsExcParser
    ) where


import Data.Functor (void)
import Data.Void

import Text.Megaparsec (ParseErrorBundle, parse, many, (<|>))

import ParsingHelpers (Parser, fully, tokenSymbol)
import UniversalParser (orParser, andParser, implicationParser, biImplicationParser, negationParser, atomParser)


operatorAndLeavesParse :: Parser ()
operatorAndLeavesParse =
  void . many $
      tokenSymbol "("
  <|> tokenSymbol ")"
  <|> orParser
  <|> andParser
  <|> implicationParser
  <|> biImplicationParser
  <|> negationParser
  <|> void atomParser

superfluousBracketsExcParser :: String -> Either (ParseErrorBundle String Void) ()
superfluousBracketsExcParser = parse (fully operatorAndLeavesParse) ""
