{-# LANGUAGE TypeApplications #-}
module ParsingSpec (spec) where

import Data.Either (isLeft, isRight)
import Test.Hspec ( describe, it, Spec)

import LogicTasks.Parsing (Parse(parser))
import LogicTasks.Formula (Cnf, Dnf, mkDnf, mkCon, Literal (Literal, Not))
import Trees.Parsing (formulaParse)
import Tasks.SuperfluousBrackets.Parsing (superfluousBracketsExcParser)

import Text.Parsec (parse)
import Formula.Types (ResStep)
import Config (dPickInst, PickInst(..), FormulaInst (InstDnf, InstArbitrary))
import Text.PrettyPrint.Leijen.Text (Pretty(pretty))
import Formula.Printing ()
import qualified Trees.Types as TT (SynTree(Binary, Leaf, Not), BinOp (And))

spec :: Spec
spec = do
  describe "formulaParse" $ do
    it "correctly recognizes simple double negation" $
      isRight $ formulaParse "~~A"
    it "correctly rejects stuff that isn't strictly well-bracketed" $
      isLeft $ formulaParse "A/\\B/\\C"
    it "correctly rejects stuff with strange spaces" $
      isLeft $ formulaParse "A/ \\B"
  describe "superfluousBracketsExcParser" $ do
    it "correctly recognizes stuff that isn't strictly well-bracketed" $
      isRight $ superfluousBracketsExcParser "A/\\B/\\C"
    it "correctly rejects stuff with strange spaces" $
      isLeft $ superfluousBracketsExcParser "A/ \\B"
  describe "parser @Literal" $ do
    it "correctly recognizes negation notations" $
      and
        [ isRight $ parse (parser @Cnf) "" "~A"
        , isRight $ parse (parser @Cnf) "" "~ B"
        , isRight $ parse (parser @Cnf) "" "-C"
        , isRight $ parse (parser @Cnf) "" "- D"
        , isRight $ parse (parser @Cnf) "" "nicht E"
        , isRight $ parse (parser @Cnf) "" "not F"
        ]
  describe "parser @Cnf" $ do
    it "correctly recognizes all different notations" $
      isRight $ parse (parser @Cnf) "" "A/\\B und (C or ~D) and (not E oder nicht F \\/ G)" {- german -}
  describe "parser @Dnf" $ do
    it "correctly recognizes all different notations" $
      isRight $ parse (parser @Dnf) "" "A \\/-B oder (C and D) or (not E and nicht F /\\ ~ G)" {- german -}

  describe "parser @ResStep" $ do
    it "can handle extra whitespace in resolved clause" $
      isRight $ parse (parser @ResStep) "" "(1, 2, {   A   ,    nicht    B   } = 5)" {- german -}
  describe "parser @PickInst" $ do
    it "correctly parses the pretty representation of a PickInst (cnf)" $
      either (const False) (== dPickInst) $ parse (parser @PickInst) "" $ show $ pretty dPickInst
    it "correctly parses the pretty representation of a PickInst (dnf)" $
      let pickInst = dPickInst { formulas = [InstDnf (mkDnf [mkCon [Literal 'A', Not 'B']])] } in
        either (const False) (== pickInst) $ parse (parser @PickInst) "" $ show $ pretty pickInst
    it "correctly parses the pretty representation of a PickInst (arbitrary)" $
      let pickInst = dPickInst { formulas = [InstArbitrary (TT.Binary TT.And (TT.Leaf 'A') (TT.Not (TT.Leaf 'B')))] } in
        either (const False) (== pickInst) $ parse (parser @PickInst) "" $ show $ pretty pickInst
