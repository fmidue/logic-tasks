{-# LANGUAGE TypeApplications #-}
module ParsingSpec (spec) where

import Data.Either (isLeft, isRight)
import Test.Hspec ( describe, it, Spec)

import LogicTasks.Parsing (Parse(parser))
import LogicTasks.Formula (Cnf, Dnf)
import Trees.Parsing (formulaParse)
import Tasks.SuperfluousBrackets.Parsing (superfluousBracketsExcParser)

import Text.Parsec (parse)
import Config (dPickInst, PickInst)
import Text.PrettyPrint.Leijen.Text (Pretty(pretty))
import Formula.Printing ()

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
  describe "parser @PickInst" $ do
    it "correctly parses the pretty representation of a PickInst" $
      either (const False) (== dPickInst) $ parse (parser @PickInst) "" $ show $ pretty dPickInst
