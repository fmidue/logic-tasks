{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Trees.Parsing (
  formulaParse,
  parseFormulaAnswer,
  parsePropForm,
  parseTreeFormulaAnswer
  ) where

import qualified Control.Applicative as Alternative (optional)

import Text.Parsec (ParseError, parse)
import Text.Parsec.String (Parser)

import Trees.Types as Formula (PropFormula(..), BinOp(..))
import Trees.Types as Tree
    ( SynTree(..)
    , FormulaAnswer(..)
    , TreeFormulaAnswer(..)
    )

import Formula.Parsing (Parse(..))

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
      , allowImplication = True
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

--------------------------------------

-- Parsers for formulas with reduced brackets
instance Parse (PropFormula Char)
instance FromGrammar (PropFormula Char) where
  topLevelSpec = spec where
    spec = LevelSpec
      { allowOr = True
      , allowAnd = True
      , allowNegation = Everywhere
      , allowAtomicProps = True
      , allowImplication = True
      , allowBiImplication = True
      , strictParens = False
      , allowSilentNesting = False
      , nextLevelSpec = Just spec
      }

  fromGrammar OfNoFixity{} = Nothing
  fromGrammar (WithPrecedence t) = fromOrs t
    where
      fromOrs (Ors f g) = Assoc Formula.Or <$> fromOrs f <*> fromAnds g
      fromOrs (OfAnds f) = fromAnds f
      fromAnds (Ands f g) = Assoc Formula.And <$> fromAnds f <*> fromImpls g
      fromAnds (OfImpl f) = fromImpls f
      fromImpls (Impls f g) = Assoc Formula.Impl <$> fromBiImpls f <*> fromImpls g
      fromImpls (OfBiImpl f) = fromBiImpls f
      fromBiImpls (BiImpls f g) = Assoc Equi <$> fromNeg f <*> fromBiImpls g
      fromBiImpls (OfNeg f) = fromNeg f
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
