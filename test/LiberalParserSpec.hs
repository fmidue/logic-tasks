{-# LANGUAGE TypeApplications #-}
module LiberalParserSpec (spec) where

import Data.Either (isLeft)
import qualified Data.Map as Map (fromList)

import Formula.Parsing (Parse(parser))

import Test.Hspec ( describe, it, shouldBe, Spec, shouldSatisfy )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (forAll, Gen, chooseInt, listOf1, elements, oneof, classify)

import Text.Parsec ( ParseError, parse )

import Tasks.SynTree.Config (defaultSynTreeConfig, SynTreeConfig (binOpFrequencies))
import Trees.Generate (genSynTree)
import Trees.Parsing ( liberalParser )
import Trees.Print (simplestDisplay)
import Trees.Types
    ( PropFormula,
      SynTree(..),
      BinOp(..),
      showOperator,
      showOperatorNot )

spec :: Spec
spec = do
  describe "liberalParser" $ do
    -- positive tests
    it "should accept \"(A ∧ B ∧ C) => D\"" $
      parseString "(A ∧ B ∧ C) => D"
      `shouldBe` Right (Binary Impl (Binary And (Binary And (Leaf 'A') (Leaf 'B')) (Leaf 'C')) (Leaf 'D'))
    it "should accept \"((A ∧ B)) => C\"" $
      parseString "((A ∧ B)) => C"
      `shouldBe` Right (Binary Impl (Binary And (Leaf 'A') (Leaf 'B')) (Leaf 'C'))
    it "should accept \"not A ∧ not (B => C) ∧ D\"" $
      parseString "not A ∧ not (B => C) ∧ D"
      `shouldBe` Right (Binary And (Binary And (Not $ Leaf 'A') (Not (Binary Impl (Leaf 'B') (Leaf 'C')))) (Leaf 'D'))
    prop "it should accept formulas of valid syntax trees with added brackets" $
      forAll treeWithExtraBrackets $ \(str,t) ->
        parseString str `shouldBe` Right t
    prop "it should accept simplest formulas of valid syntax trees" $
      forAll simplestTree $ \str ->
        (simplestDisplay <$> parseString str) `shouldBe` Right str

    -- negative tests
    it "should reject \"A ∧ B => C\"" $
      parseString "A ∧ B => C" `shouldSatisfy` isLeft
    it "should reject \"A => B => C\"" $
      parseString "A => B => C" `shouldSatisfy` isLeft
    it "should reject \"A <= B <= C\"" $
      parseString "A <= B <= C" `shouldSatisfy` isLeft
    it "should reject \"A <=> B <=> C\"" $
      parseString "A <=> B <=> C" `shouldSatisfy` isLeft
    it "should reject \"A ∧ B ∨ C\"" $
      parseString "A ∧ B ∨ C" `shouldSatisfy` isLeft
    it "should reject \"A ∧ not B => C ∧ D\"" $
      parseString "A ∧ not B => C ∧ D" `shouldSatisfy` isLeft
    prop "it should agree with PropFormula-parser on which token sequences are valid formulas" $
      forAll tokenSequence $ \str ->
        let propFormulaFailure = isLeft $ parse (parser @(PropFormula Char)) "(reference)" str
        in classify propFormulaFailure "parser errors" $ isLeft (parseString str) `shouldBe` propFormulaFailure

parseString :: String -> Either ParseError (SynTree BinOp Char)
parseString = parse liberalParser "(test case)"

treeWithExtraBrackets :: Gen (String,SynTree BinOp Char)
treeWithExtraBrackets = do
  t <- genSynTree defaultSynTreeConfig
      { binOpFrequencies = Map.fromList
        [ (And, 1)
        , (Or, 1)
        , (Impl, 1)
        , (BackImpl, 1)
        , (Equi, 1)
        ]
      }
  str <- showWithExtraBrackets t
  pure (str,t)

simplestTree :: Gen String
simplestTree = do
  t <- genSynTree defaultSynTreeConfig
      { binOpFrequencies = Map.fromList
        [ (And, 1)
        , (Or, 1)
        , (Impl, 1)
        , (BackImpl, 1)
        , (Equi, 1)
        ]
      }
  pure $ simplestDisplay t

showWithExtraBrackets :: SynTree BinOp Char -> Gen String
showWithExtraBrackets = showWithExtraBrackets' ("","")

showWithExtraBrackets' :: (String,String) -> SynTree BinOp Char -> Gen String
showWithExtraBrackets' _ (Leaf a) = addSomeBrackets [a]
showWithExtraBrackets' _ (Not (Leaf a)) = do
  str <- addSomeBrackets [a]
  addSomeBrackets $ showOperatorNot <> str
showWithExtraBrackets' _ (Not a) = do
  str <- showWithExtraBrackets' ("(",")") a
  addSomeBrackets $ showOperatorNot <> str
showWithExtraBrackets' (l,r) (Binary operator a b) = do
  strA <- case a of
    Leaf _ -> showWithExtraBrackets' ("","") a
    _ -> showWithExtraBrackets' ("(",")") a
  strB <- case b of
    Leaf _ -> showWithExtraBrackets' ("","") b
    _ -> showWithExtraBrackets' ("(",")") b
  addSomeBrackets $ concat [l,strA, " ", showOperator operator, " ", strB,r]

addSomeBrackets :: String -> Gen String
addSomeBrackets x = do
  n <- chooseInt (0,3)
  pure $ concat [replicate n '(', x, replicate n ')']

tokenSequence :: Gen String
tokenSequence = unwords <$> listOf1
  (oneof
    [ pure showOperatorNot
    , pure $ showOperator And
    , pure $ showOperator Or
    , pure $ showOperator Impl
    , pure $ showOperator BackImpl
    , pure <$> elements "ABCDE"
    ]
  )
