{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Trees.Parsing (
  formulaParse,
  parseFormulaAnswer,
  parsePropForm,
  parseTreeFormulaAnswer,
  liberalParser,
  ) where

import qualified Control.Applicative as Alternative (optional)

import Data.List (nub, intercalate)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty

import Text.Parsec (ParseError, parse)
import Text.Parsec.String (Parser)

import Trees.Types as Formula (PropFormula(..), BinOp(..))
import Trees.Types as Tree
    ( SynTree(..)
    , FormulaAnswer(..)
    , TreeFormulaAnswer(..), toLayeredTree, validateLayers, unfoldLayers, showOperator,
    )

import Formula.Parsing.Type (Parse(..))

import ParsingHelpers (fully)
import UniversalParser as Parser

instance Parse (SynTree BinOp Char)
instance FromGrammar (SynTree BinOp Char) where
  topLevelSpec = spec where
    spec = LevelSpec
      { allowOr = True
      , allowAnd = True
      , allowNegation = Everywhere
      , allowAtomicProps = True
      , allowImplication = Both
      , allowBiImplication = True
      , strictParens = True
      , allowSilentNesting = False
      , nextLevelSpec = Just spec
      }

  fromGrammar WithPrecedence{} = Nothing
  fromGrammar (OfNoFixity t) = fromNoFixity t
    where
      fromNoFixity (NoFixity f op g) = Binary (fromOp op) <$> fromBasic f <*> fromBasic g
      fromNoFixity (OfBasic f) = fromBasic f
      fromBasic (BasicNested f) = fromNested f
      fromBasic (BasicNeg f) = fromNeg f
      fromOp Parser.Or = Formula.Or
      fromOp Parser.And = Formula.And
      fromOp Parser.Impl = Formula.Impl
      fromOp Parser.BackImpl = Formula.BackImpl
      fromOp BiImpl = Equi
      fromNeg :: Neg -> Maybe (SynTree BinOp Char)
      fromNeg (NegAtom (Atom x)) = Just $ Not $ Leaf x
      fromNeg (OfAtom (Atom x)) = Just $ Leaf x
      fromNeg (Parser.Neg f) = Not <$> fromNeg f
      fromNeg (OfNested f) = fromNested f
      fromNested (Nested f) = fromGrammar f

formulaParse :: String -> Either ParseError (SynTree BinOp Char)
formulaParse = parse (fully formulaParser) ""

instance Parse TreeFormulaAnswer where
  parser = TreeFormulaAnswer <$> Alternative.optional parser

{-# DEPRECATED parseTreeFormulaAnswer "use Parse instance" #-}
parseTreeFormulaAnswer :: Parser TreeFormulaAnswer
parseTreeFormulaAnswer = parser

--------------------

-- Parser for formulas with more liberal bracket requirements:
-- Sequences of only conjunctions or disjunctions can be written
-- without brackets, i.e., A ∧ B ∧ C instead of (A ∧ B) ∧ C.
-- No operator precedence!
liberalParser :: Parser (SynTree BinOp Char)
liberalParser = do
  f <- parser :: Parser (PropFormula Char)
  case validateLayers isValidLayer (toLayeredTree f) of
    Right t -> pure $ unfoldLayers t
    Left err -> fail err

isValidLayer :: NonEmpty BinOp -> Maybe String
isValidLayer (_ :| []) = Nothing
isValidLayer (Formula.And :| xs)
  | isConjunction xs = Nothing
  | otherwise = Just $ reportError $ Formula.And:xs
isValidLayer (Formula.Or :| xs)
  | isDisjunction xs = Nothing
  | otherwise = Just $ reportError $ Formula.Or:xs
isValidLayer xs = Just $ reportError $ NonEmpty.toList xs

isConjunction :: [BinOp] -> Bool
isConjunction = all (== Formula.And)

isDisjunction :: [BinOp] -> Bool
isDisjunction = all (== Formula.Or)

reportError :: [BinOp] -> String
reportError xs =
  "unexpected mixing of operators without parenthesis: " <>
  intercalate ", " (map showOperator $ nub xs)

--------------------------------------

-- Parsers for formulas with reduced brackets
-- (missing) brackets are inferred through operator precedence
instance Parse (PropFormula Char)
instance FromGrammar (PropFormula Char) where
  topLevelSpec = spec where
    spec = LevelSpec
      { allowOr = True
      , allowAnd = True
      , allowNegation = Everywhere
      , allowAtomicProps = True
      , allowImplication = Both
      , allowBiImplication = True
      , strictParens = False
      , allowSilentNesting = False
      , nextLevelSpec = Just spec
      }

  fromGrammar OfNoFixity{} = Nothing
  fromGrammar (WithPrecedence t) = fromBiImpls t
    where
      fromOrs (Ors f g) = Assoc Formula.Or <$> fromOrs f <*> fromAnds g
      fromOrs (OfAnds f) = fromAnds f
      fromAnds (Ands f g) = Assoc Formula.And <$> fromAnds f <*> fromNeg g
      fromAnds (OfNeg f) = fromNeg f
      fromImpls (Impls f g) = Assoc Formula.Impl <$> fromOrs f <*> fromImpls g
      fromImpls (BackImpls f g) = Assoc Formula.BackImpl <$> fromOrs f <*> fromImpls g
      fromImpls (OfOrs f) = fromOrs f
      fromBiImpls (BiImpls f g) = Assoc Equi <$> fromImpls f <*> fromBiImpls g
      fromBiImpls (OfImpls f) = fromImpls f
      fromNeg (NegAtom (Atom x)) = Just $ Formula.Neg $ Atomic x
      fromNeg (OfAtom (Atom x)) = Just $ Atomic x
      fromNeg (Parser.Neg f) = Formula.Neg <$> fromNeg f
      fromNeg (OfNested f) = fromNested f
      fromNested :: Nested -> Maybe (PropFormula Char)
      fromNested (Nested f) = Brackets <$> fromGrammar f

{-# DEPRECATED parsePropForm "use Parse instance" #-}
parsePropForm :: Parser (PropFormula Char)
parsePropForm = parser

instance Parse FormulaAnswer where
  parser = FormulaAnswer <$> Alternative.optional parser

{-# DEPRECATED parseFormulaAnswer "use Parse instance" #-}
parseFormulaAnswer :: Parser FormulaAnswer
parseFormulaAnswer = parser
