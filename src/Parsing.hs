{-# language RecordWildCards #-}

module Parsing where


import Config
import Formula
import Types

import Text.ParserCombinators.Parsec




parseOr :: Parser ()
parseOr = do
    spaces
    string "\\/"
    spaces


parseAnd :: Parser ()
parseAnd = do
    spaces
    string "/\\"
    spaces


parseList :: Parser a -> Parser [a]
parseList p = do
    spaces
    char '['
    spaces
    xs <- p `sepBy` (char ',')
    spaces
    char ']'
    return xs






class Parse a where
  parser :: Parser a



instance Parse Number where
  parser = do
      result <- optionMaybe $ many1 digit
      return $ Number $ fmap read result




instance Parse Literal where
  parser = do
      spaces
      result <- optionMaybe $ char '~'
      var <- satisfy $ flip elem ['A'..'Z']
      spaces
      case result of Nothing -> return (Literal var)
                     Just _  -> return (Not var)




instance Parse Clause where
 parser = do
    spaces
    braces <- optionMaybe $ char '('
    spaces
    lits <- sepBy parser parseOr
    spaces
    case braces of Nothing -> return ' '
                   Just _ -> char ')'
    spaces
    return $ mkClause lits



instance Parse Cnf where
  parser = do
      spaces
      clauses <- sepBy parser parseAnd
      spaces
      return $ mkCnf clauses



instance Parse Table



instance Parse PickInst where
  parser = do
      string "PickInst("
      spaces
      cnfs <- parseList parser
      spaces
      char ','
      spaces
      index <- many1 digit
      spaces
      text <- optionMaybe extraText
      spaces
      char ')'
      return $ PickInst cnfs (read index) text
    where
      extraText = between start (char '}') $ many1 $ satisfy ( /= '}')
      start = do
          char ','
          spaces
          char '{'






