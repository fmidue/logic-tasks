{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module UniversalParser where

import Data.Functor (($>), void)
import Data.Maybe (fromMaybe)

import Text.Parsec (satisfy, (<|>), (<?>), choice, try, unexpected, lookAhead, char, many)
import Text.Parsec.String (Parser)

import ParsingHelpers

{-- Universal parser for the following grammar of propositional formulas:
  Formula ::= NoFixity | Ors

  -- no implicit parenthesis
  NoFixity ::= Basic Op Basic | Basic
  Basic ::= Nested | Neg
  Op ::= ∨ | ∧ | => | <=>

  -- with precedence
  Ors ::= Ors ∨ Ands | Ands
  Ands ::= Ands ∧ Impl | Impl
  Impl ::= BiImpl => Impl | BiImpl
  BiImpl ::= Neg <=> BiImpl | Neg

  -- 'leaf' formulas
  Neg ::= ¬ Atom | Atom | ¬ Neg | Nested
  Nested ::= ( Formula )
  Atom ::= A | .. | Z

  Individual grammar productions can be toggled off (at specific nesting depths)
  to parse specialized subsets of formulas.
--}

-- types for universal grammar
data FormulaGrammar = WithPrecedence Ors | OfNoFixity NoFixity
  deriving Show

data NoFixity = NoFixity Basic Op Basic | OfBasic Basic
  deriving Show

data Basic = BasicNested Nested | BasicNeg Neg
  deriving Show

data Op = Or | And | Impl | BiImpl
  deriving Show

data Ors = Ors Ors Ands | OfAnds Ands
  deriving Show

data Ands = Ands Ands Impls | OfImpl Impls
  deriving Show

data Impls = Impls BiImpls Impls | OfBiImpl BiImpls
  deriving Show

data BiImpls = BiImpls Neg BiImpls | OfNeg Neg
  deriving Show

data Neg = NegAtom Atom | OfAtom Atom | Neg Neg | OfNested Nested
  deriving Show

newtype Atom = Atom Char
  deriving Show

newtype Nested = Nested FormulaGrammar
  deriving Show

-- folds over binary operators
foldlOrs :: (a -> Ands -> a) -> a -> Ors -> a
foldlOrs f z (Ors x y) = foldlOrs f z x `f` y
foldlOrs f z (OfAnds x) = f z x

foldlAnds :: (a -> Impls -> a) -> a -> Ands -> a
foldlAnds f z (Ands x y) = foldlAnds f z x `f` y
foldlAnds f z (OfImpl x) = f z x

foldrImpl :: (BiImpls -> a -> a) -> a -> Impls -> a
foldrImpl f z (Impls x y) = f x $ foldrImpl f z y
foldrImpl f z (OfBiImpl x) = f x z

foldrBiImpl :: (Neg -> a -> a) -> a -> BiImpls -> a
foldrBiImpl f z (BiImpls x y) = f x $ foldrBiImpl f z y
foldrBiImpl f z (OfNeg x) = f x z

-- useful pattern synonyms
{-# COMPLETE Ors, NoOrs #-}
pattern NoOrs :: Ands -> Ors
pattern NoOrs x = OfAnds x

{-# COMPLETE Ands, NoAnds #-}
pattern NoAnds :: Impls -> Ands
pattern NoAnds x = OfImpl x

{-# COMPLETE Impls, NoImpls #-}
pattern NoImpls :: BiImpls -> Impls
pattern NoImpls x = OfBiImpl x

{-# COMPLETE BiImpls, NoBiImpls #-}
pattern NoBiImpls :: Neg -> BiImpls
pattern NoBiImpls x = OfNeg x

{-# COMPLETE Impls, NoImplBiImpl, NoArrows #-}
pattern NoImplBiImpl :: Impls
pattern NoImplBiImpl <- NoImpls (BiImpls _ _)

pattern NoArrows :: Neg -> Impls
pattern NoArrows x = NoImpls (NoBiImpls x)

{-# COMPLETE Ors, SkipLevel #-}
pattern SkipLevel :: FormulaGrammar -> Ors
pattern SkipLevel x = NoOrs (NoAnds (NoArrows (OfNested (Nested x))))

-- parser configuration
data LevelSpec = LevelSpec
  { allowOr :: Bool
  , allowAnd :: Bool
  , allowNegation :: AllowNegation
  , allowAtomicProps :: Bool
  , allowImplication :: Bool
  , allowBiImplication :: Bool
  , strictParens :: Bool -- strict parenthesis will trigger excessive nesting and thereby level changes!!!
  , allowSilentNesting :: Bool -- DANGER!!!
  , nextLevelSpec :: Maybe LevelSpec
  }

data AllowNegation = Nowhere | LiteralsOnly | Everywhere

-- parser for operations
orParser :: Parser ()
orParser =
      keyword "or"
  <|> keyword "oder" {- german -}
  <|> tokenSymbol "∨" <|> tokenSymbol "\\/"
  <?> "Disjunction"

andParser :: Parser ()
andParser =
      keyword "and"
  <|> keyword "und" {- german -}
  <|> tokenSymbol "∧" <|> tokenSymbol "/\\"
  <?> "Conjunction"

implicationParser :: Parser ()
implicationParser = tokenSymbol "=>" <?> "Implication"

biImplicationParser :: Parser ()
biImplicationParser =
      tokenSymbol "<=>"
  <|> keyword "iff" <|> keyword "gdw"
  <?> "BiImplication"

negationParser :: Parser ()
negationParser =
      keyword "not" <|> keyword "nicht"
  <|> tokenSymbol "¬" <|> tokenSymbol "~" <|> tokenSymbol "-"
  <?> "Negation"

atomParser :: Parser Char
atomParser = token (satisfy (`elem` ['A'..'Z'])) <?> "atomic Proposition"

-- parser for token sequences
tokenSequence :: Parser ()
tokenSequence = void $ many $
      orParser
  <|> andParser
  <|> implicationParser
  <|> biImplicationParser
  <|> negationParser
  <|> void atomParser
  <|> tokenSymbol "("
  <|> tokenSymbol ")"

-- the universal parser
formula :: LevelSpec -> Parser FormulaGrammar
formula LevelSpec{..}
  | strictParens = OfNoFixity <$> noFixity
  | otherwise = WithPrecedence <$> ors
  where
  noFixity :: Parser NoFixity
  noFixity = do
    x <- basic
    NoFixity x <$> anyBinaryOp <*> basic <|> pure (OfBasic x)

  anyBinaryOp :: Parser Op
  anyBinaryOp = _noNested *> chooseBinary <* _noSecondOp
    where
    chooseBinary :: Parser Op
    chooseBinary = choice
      (  [ Or <$ orParser | allowOr ]
      ++ [ And <$ andParser | allowAnd ]
      ++ [ Impl <$ implicationParser | allowImplication ]
      ++ [ BiImpl <$ biImplicationParser | allowBiImplication ]
      )

    _noNested :: Parser () -- catch the A (B and C) error pattern
    _noNested = (do
      lookAhead $ char '('
      _ <- try nested
      unexpected "formula"
      fail "maybe there is an operator missing")
      <|> pure ()

    _noSecondOp :: Parser ()
    _noSecondOp = (do
      op <- try chooseBinary
      unexpected $ "second operator: " ++ case op of
        Or -> "Disjunction"
        And -> "Conjunction"
        Impl -> "Implication"
        BiImpl -> "Bi-Implication")
      <|> pure ()

  basic :: Parser Basic
  basic = BasicNested <$> nested <|> BasicNeg <$> neg

  ors :: Parser Ors
  ors
    | allowOr = infixl1 OfAnds ands (orParser $> Ors)
    | otherwise = OfAnds <$> ands

  ands :: Parser Ands
  ands
    | allowAnd = infixl1 OfImpl impl (andParser $> Ands)
    | otherwise = OfImpl <$> impl

  impl :: Parser Impls
  impl
    | allowImplication = infixr1 OfBiImpl biImpl (implicationParser $> Impls)
    | otherwise = OfBiImpl <$> biImpl

  biImpl :: Parser BiImpls
  biImpl
    | allowBiImplication = infixr1 OfNeg neg (biImplicationParser $> BiImpls)
    | otherwise = OfNeg <$> neg

  neg :: Parser Neg
  neg = case allowNegation of
    Everywhere ->
          plainNested
      <|> plainAtom
      <|> negationParser *> nextNeg
      where nextNeg = NegAtom <$> atom <|> Neg . OfNested <$> nested  <|> Neg <$> neg
    LiteralsOnly ->
          plainNested
      <|> plainAtom
      <|> negationParser $> NegAtom <*> atom
    Nowhere -> plainNested <|> plainAtom
    where
      plainAtom = OfAtom <$> atom
      plainNested = OfNested <$> nested

  atom :: Parser Atom
  atom
    | allowAtomicProps = Atom <$> atomParser
    | otherwise = fail "no atomic propositions allowed at this level"

  nested :: Parser Nested
  nested
    | allowSilentNesting = Nested <$> silentNested <|> Nested <$> innerNested
    | otherwise = Nested <$> innerNested
    where
      innerNested =
        maybe
          (fail "term structure does not allow nesting at this point")
          (parens . formula)
          nextLevelSpec
      silentNested =
        maybe
          (fail "term structure does not allow nesting at this point")
          formula
          nextLevelSpec

-- type class for parseable types
class FromGrammar a where
  topLevelSpec :: LevelSpec
  fromGrammar :: FormulaGrammar -> Maybe a

formulaParser :: forall a. FromGrammar a => Parser a
formulaParser =
  fromMaybe (error "contradiction between parser rules and fromGrammar") . fromGrammar <$> formula (topLevelSpec @a)
