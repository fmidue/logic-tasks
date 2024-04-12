{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Formula.Parsing where

import Config
import Formula.Util
import ParsingHelpers (lexeme, tokenSymbol)
import Formula.Types

import Control.Monad (void)
import Data.Char (toLower)
import Data.Map (fromList)
import Text.ParserCombinators.Parsec (
  Parser,
  (<?>),
  (<|>),
  alphaNum,
  between,
  char,
  digit,
  getInput,
  many1,
  notFollowedBy,
  optionMaybe,
  satisfy,
  sepBy,
  setInput,
  spaces,
  string,
  try,
  )

import UniversalParser

instance Parse ResStep where
  parser = do
    tokenSymbol "("
    cl1 <- parseEither resClause parseNum
    tokenSymbol ","
    cl2 <- parseEither resClause parseNum
    tokenSymbol ","
    cl3 <- resClause
    index <- optionMaybe indexParse
    tokenSymbol ")"
    pure $ Res (cl1,cl2,(cl3,index))

   where
    braces = between (tokenSymbol "{") (tokenSymbol "}")

    indexParse = tokenSymbol "=" >> lexeme parseNum

    resClause = mkClause <$> braces (parser `sepBy` char ',')

    parseEither x y = lexeme ((Left <$> try x) <|> (Right <$> y))

    parseNum = do
      i <- many1 digit
      pure (read i)

notFollowedByElse :: Parser a -> (a -> Parser ()) -> Parser ()
notFollowedByElse p f = try ((try p >>= f) <|> pure ())



class Parse a where
  parser :: Parser a
  default parser :: FromGrammar a => Parser a
  parser = formulaParser


instance Parse a => Parse [a] where
  parser = (lexeme listParse <?> "List")
      <|> fail (
        "Could not parse a list of values: " ++
        "The elements of a list are enclosed by square brackets '[ ]' and separated by commas."
        )
    where
      listParse = do
        tokenSymbol "[" <|> fail "could not parse an opening '['"
        xs <- parser `sepBy` (tokenSymbol "," <|> fail "parsed a wrong separator: Lists are comma-separated.")
        tokenSymbol "]" <|> fail "could not parse an enclosing ']'"
        pure xs

instance (Parse a, Parse b) => Parse (a,b) where
  parser = between (tokenSymbol "(") (tokenSymbol ")") $ do
    a <- parser
    tokenSymbol ","
    b <- parser
    pure (a,b)

instance Parse Number where
  parser = (lexeme numParse <?> "Number") <|> fail "Could not parse a number"
    where numParse = do
            result <- optionMaybe $ many1 digit
            pure $ Number $ fmap read result


instance Parse StepAnswer where
  parser = (lexeme stepParse <?> "Resolution Step") <|> fail "Could not parse a tuple"
    where stepParse = do
            result <- optionMaybe parseTuple
            pure $ StepAnswer result
          parseTuple = do
            void $ lexeme $ char '('
            lit <- parser
            void $ lexeme $ char ','
            resolvent <- parser
            void $ lexeme $ char ')'
            pure (lit, resolvent)




instance Parse TruthValue where
  parser = lexeme truthParse <?> "Truth Value"

    where truthParse = do
            s <- getInput
            setInput (map toLower s)
            t <- try
             (    parseTrue
              <|> parseFalse
              <|> fail "Could not parse a truth value: Please enter values as described in the exercise description."
             )
              <|> fail "The truth value was mistyped."
            notFollowedByElse alphaNum (\c ->
              fail $ unlines
                [ "unexpected " ++ [c]
                , "Additional characters were appended to this truth value or it was mistyped."
                ]
              )
            pure t
              where
                parseTrue = do
                  string "1" <|> try (single "w") <|> try (single "t")
                    <|> string "wahr" <|> string "true" -- no-spell-check
                  pure $ TruthValue True
                parseFalse = do
                  string "0" <|> try (single "f") <|> eitherDeEn
                  pure $ TruthValue False

                single :: String -> Parser String
                single s = do
                    res <- string s
                    notFollowedBy alphaNum
                    return res

                eitherDeEn = string "fals" >> (try (string "e") <|> string "ch") -- no-spell-check





instance Parse Literal
instance FromGrammar Literal where
  topLevelSpec = LevelSpec
    { allowOr = False
    , allowAnd = False
    , allowNegation = LiteralsOnly
    , allowAtomicProps = True
    , allowImplication = False
    , strictParens = False
    , allowBiImplication = False
    , allowSilentNesting = False
    , nextLevelSpec = Nothing
    }

  fromGrammar (WithPrecedence (NoOrs (NoAnds (NoArrows (OfAtom (Atom x)))))) = Just $ Literal x
  fromGrammar (WithPrecedence (NoOrs (NoAnds (NoArrows (NegAtom (Atom x)))))) = Just $ Not x
  fromGrammar _ = Nothing

instance Parse Clause where
  parser = mkClause [] <$ tokenSymbol "{}" <|> formulaParser

instance FromGrammar Clause where
  topLevelSpec = LevelSpec
    { allowOr = True
    , allowAnd = False
    , allowNegation = LiteralsOnly
    , allowAtomicProps = True
    , allowImplication = False
    , allowBiImplication = False
    , strictParens = False
    , allowSilentNesting = False
    , nextLevelSpec = Nothing
    }

  fromGrammar OfNoFixity{} = Nothing
  fromGrammar (WithPrecedence f) =  mkClause <$> foldlOrs phi (Just []) f
    where
      phi :: Maybe [Literal] -> Ands -> Maybe [Literal]
      phi xs (NoAnds (NoArrows (OfAtom (Atom x)))) = (Literal x :) <$> xs
      phi xs (NoAnds (NoArrows ((NegAtom (Atom x))))) = (Not x :) <$> xs
      phi _ (NoAnds (NoArrows (Neg{}))) = Nothing
      phi _ (NoAnds (NoArrows (OfNested{}))) = Nothing
      phi _ (NoAnds NoImplBiImpl) = Nothing
      phi _ (NoAnds Impls{}) = Nothing
      phi _ Ands{} = Nothing

instance Parse Con
instance FromGrammar Con where
  topLevelSpec = LevelSpec
    { allowOr = False
    , allowAnd = True
    , allowNegation = LiteralsOnly
    , allowAtomicProps = True
    , allowImplication = False
    , allowBiImplication = False
    , strictParens = False
    , allowSilentNesting = False
    , nextLevelSpec = Nothing
    }

  fromGrammar (WithPrecedence Ors{}) = Nothing
  fromGrammar OfNoFixity{} = Nothing
  fromGrammar (WithPrecedence (OfAnds f)) = mkCon <$> foldlAnds phi (Just []) f
    where
      phi :: Maybe [Literal] -> Impls -> Maybe [Literal]
      phi xs (NoArrows (OfAtom (Atom x))) = (Literal x :) <$> xs
      phi xs (NoArrows (NegAtom (Atom x))) = (Not x :) <$> xs
      phi _ (NoArrows Neg{}) = Nothing
      phi _ (NoArrows OfNested{}) = Nothing
      phi _ NoImplBiImpl = Nothing
      phi _ Impls{} = Nothing

instance Parse Cnf
instance FromGrammar Cnf where
  topLevelSpec = (topLevelSpec @Con) { allowSilentNesting = True, nextLevelSpec = Just $ topLevelSpec @Clause }

  fromGrammar = (mkCnf <$>) . go
    where
      go (WithPrecedence (OfAnds f)) = foldlAnds phi (Just []) f
      go (WithPrecedence (SkipLevel f)) = pure <$> fromGrammar @Clause f
      go (WithPrecedence (Ors{})) = Nothing
      go OfNoFixity{} = Nothing
      phi :: Maybe [Clause] -> Impls -> Maybe [Clause]
      phi xs (NoArrows (OfNested (Nested f))) = (:) <$> fromGrammar f  <*> xs
      phi xs (NoArrows (OfAtom (Atom x))) = (mkClause [Literal x] :) <$> xs
      phi xs (NoArrows (NegAtom (Atom x))) = (mkClause [Not x] :) <$> xs
      phi _ (NoArrows Neg{}) = Nothing
      phi _ NoImplBiImpl = Nothing
      phi _ Impls{} = Nothing

instance Parse Dnf
instance FromGrammar Dnf where
  topLevelSpec = (topLevelSpec @Clause) { allowSilentNesting = True, nextLevelSpec = Just $ topLevelSpec @Con }

  fromGrammar OfNoFixity{} = Nothing
  fromGrammar (WithPrecedence f) = mkDnf <$> foldlOrs phi (Just []) f
    where
      phi :: Maybe [Con] -> Ands -> Maybe [Con]
      phi xs (NoAnds (NoArrows (OfNested (Nested x)))) = (:) <$> fromGrammar x <*> xs
      phi xs (NoAnds (NoArrows (OfAtom (Atom x)))) = (mkCon [Literal x] :) <$> xs
      phi xs (NoAnds (NoArrows (NegAtom (Atom x)))) = (mkCon [Not x] :) <$> xs
      phi _ (NoAnds (NoArrows Neg{})) = Nothing
      phi _ (NoAnds Impls{}) = Nothing
      phi _ (NoAnds NoImplBiImpl) = Nothing
      phi _ (Ands{}) = Nothing


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
       ts <- sepBy parser orParser
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
        tokenSymbol ","
        index <- lexeme $ many1 digit
        printSol <- lexeme text'
        bonusText <- optionMaybe $ lexeme text'
        char ')'
        pure $ PickInst cs (read index) (read printSol) (fromList . read <$> bonusText)
          where
            text' = between start (char '}') $ many1 $ satisfy ( /= '}')
            start = do
              char ','
              spaces
              char '{'
