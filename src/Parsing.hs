
module Parsing where


import Config
import Formula
import ParsingHelpers (lexeme)
import Types

import Control.Monad (void)
import Data.Char (toLower)
import Text.ParserCombinators.Parsec



instance Parse ResStep where
  parser = do
    withSpaces '('
    cl1 <- parseEither resClause parseNum
    withSpaces ','
    cl2 <- parseEither resClause parseNum
    withSpaces ','
    cl3 <- resClause
    index <- optionMaybe indexParse
    withSpaces ')'
    pure $ Res (cl1,cl2,(cl3,index))

   where
    braces = between (withSpaces '{') (withSpaces '}')

    indexParse = withSpaces '=' >> lexeme parseNum

    resClause = mkClause <$> braces (parser `sepBy` char ',')

    parseEither x y = lexeme ((Left <$> try x) <|> (Right <$> y))

    parseNum = do
      i <- many1 digit
      pure (read i)



withSpaces :: Char -> Parser Char
withSpaces = lexeme . char


parseOr :: Parser ()
parseOr = (lexeme (void $ string "\\/") <?> "Disjunction") <|> fail "Could not parse a disjunction (\\/)"



parseAnd :: Parser ()
parseAnd = (lexeme (void $ string "/\\") <?> "Conjunction") <|> fail "Could not parse a conjunction (/\\)"


notFollowedByElse :: Parser a -> (a -> Parser ()) -> Parser ()
notFollowedByElse p f = try ((try p >>= f) <|> pure ())



class Parse a where
  parser :: Parser a



instance Parse a => Parse [a] where
  parser = (lexeme listParse <?> "List")
           <|> fail "Could not parse a list of values: The elements of a list are enclosed by square brackets '[ ]' and separated by commas."
    where
      listParse = do
        withSpaces '[' <|> fail "could not parse an opening '['"
        xs <- parser `sepBy` (withSpaces ',' <|> fail "parsed a wrong separator: Lists are comma-separated.")
        withSpaces ']' <|> fail "could not parse an enclosing ']'"
        pure xs



instance Parse Number where
  parser = (lexeme numParse <?> "Number") <|> fail "Could not parse a number"
    where numParse = do
            result <- optionMaybe $ many1 digit
            pure $ Number $ fmap read result




instance Parse TruthValue where
  parser = lexeme truthParse <?> "Truth Value"

    where truthParse = do
            s <- getInput
            setInput (map toLower s)
            t <- try (parseTrue <|> parseFalse <|> fail "Could not parse a truth value: Please enter values as described in the exercise description.")
                      <|> fail "The truth value was mistyped."
            notFollowedByElse alphaNum (\c -> fail $ unlines
                                               ["unexpected " ++ [c]
                                               ,"Additional characters were appended to this truth value or it was mistyped."
                                               ])
            pure t
              where
                parseTrue = do
                  string "1" <|> try (single "w") <|> try (single "t") <|> string "wahr" <|> string "true"
                  pure $ TruthValue True
                parseFalse = do
                  string "0" <|> try (single "f") <|> eitherDeEn
                  pure $ TruthValue False

                single :: String -> Parser String
                single s = do
                    res <- string s
                    notFollowedBy alphaNum
                    return res

                eitherDeEn = string "fals" >> (try (string "e") <|> string "ch")





instance Parse Literal where
  parser = (lexeme litParse <?> "Literal")
           <|> fail "Could not parse a literal: Literals are denoted by capital letters, negation is denoted by a '~'."
    where
      litParse = do
        result <- optionMaybe $ char '~'
        var <- satisfy $ flip elem ['A'..'Z']
        case result of Nothing -> pure (Literal var)
                       Just _  -> pure (Not var)




instance Parse Clause where
 parser = (lexeme clauseParse <?> "Clause")
          <|> fail "Could not parse a clause: Clauses are composed out of literals and the 'or operator' (\\/)."
   where
     clauseParse = do
       braces <- lexeme $ optionMaybe $ char '('
       lits <- sepBy parser parseOr
       case braces of Nothing -> pure ' '
                      Just _ -> char ')'
       pure $ mkClause lits



instance Parse Con where
 parser = (lexeme conParse <?> "Conjunction")
          <|> fail "Could not parse a conjunction: Conjunctions are composed out of literals and the 'and operator' (/\\)."
   where
     conParse = do
       braces <- lexeme $ optionMaybe $ char '('
       lits <- sepBy parser parseAnd
       case braces of Nothing -> pure ' '
                      Just _ -> char ')'
       pure $ mkCon lits



instance Parse Cnf where
  parser = (lexeme parseCnf <?> "CNF")
           <|> fail "Could not parse a CNF: CNFs are composed out of clauses and the 'and operator' (/\\)."
    where
      parseCnf = do
        cls <- sepBy parser parseAnd
        pure $ mkCnf cls



instance Parse Dnf where
  parser = (lexeme parseDnf <?> "DNF")
           <|> fail "Could not parse a DNF: DNFs are composed out of clauses and the 'or operator' (\\/)."
    where
      parseDnf = do
        cons <- sepBy parser parseOr
        pure $ mkDnf cons



instance Parse PrologLiteral where
  parser = (lexeme litParse <?> "Literal")
           <|> fail "Could not parse a literal."
    where
      litParse = do
        pol <- lexeme $ optionMaybe $ string "not("
        ident <- strParse
        lexeme $ char '('
        facts <- lexeme $ sepBy (lexeme strParse) (lexeme $ char ',')
        lexeme $ char ')'
        case pol of Nothing -> pure (PrologLiteral True ident facts)
                    Just _  -> do char ')'
                                  pure (PrologLiteral False ident facts)
        where
          strParse = many1 $ satisfy $ flip elem ['A'..'z']




instance Parse PrologClause where
 parser = (lexeme (emptyParse <|> clauseParse) <?> "Clause")
          <|> fail "Could not parse a clause: Clauses are composed out of terms and the 'or operator' (\\/)."
   where
     clauseParse = do
       braces <- lexeme $ optionMaybe $ char '('
       ts <- sepBy parser parseOr
       case braces of Nothing -> pure ' '
                      Just _ -> char ')'
       pure $ mkPrologClause ts
     emptyParse = do
       char '{'
       spaces
       char '}'
       pure $ mkPrologClause []


instance Parse PickInst where
  parser = lexeme instParse
    where
      instParse = do
        string "PickInst("
        cs <- parser
        withSpaces ','
        index <- lexeme $ many1 digit
        text <- optionMaybe $ lexeme bonusText
        char ')'
        pure $ PickInst cs (read index) text
          where
            bonusText = between start (char '}') $ many1 $ satisfy ( /= '}')
            start = do
              char ','
              spaces
              char '{'
