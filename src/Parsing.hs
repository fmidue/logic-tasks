{-# LANGUAGE DefaultSignatures #-}
module Parsing where

import Text.ParserCombinators.Parsec (Parser)
import UniversalParser


class Parse a where
  parser :: Parser a
  default parser :: FromGrammar a => Parser a
  parser = formulaParser
