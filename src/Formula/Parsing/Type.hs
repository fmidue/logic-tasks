{-# LANGUAGE DefaultSignatures #-}
module Formula.Parsing.Type where

import UniversalParser
import Text.ParserCombinators.Parsec (Parser)

class Parse a where
  parser :: Parser a
  default parser :: FromGrammar a => Parser a
  parser = formulaParser
