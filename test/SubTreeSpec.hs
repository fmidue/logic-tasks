{-# LANGUAGE RecordWildCards, NamedFieldPuns, TypeApplications #-}
module SubTreeSpec (spec) where

import Test.Hspec (describe, it, xit, Spec)
import Test.QuickCheck (Gen, choose, forAll, elements, suchThat)
import Text.Parsec (parse)
import Data.Either.Extra (fromRight')
import Data.List.Extra (isInfixOf )
import Data.Set (size)
import qualified Data.Set (map)

import Tasks.SubTree.Config (SubTreeConfig(..), SubTreeInst(..), checkSubTreeConfig, defaultSubTreeConfig)
import Tasks.SubTree.Quiz (generateSubTreeInst)
import Trees.Helpers (allNotLeafSubTrees, maxLeavesForNodes)
import Tasks.SynTree.Config (SynTreeConfig(..),)
import TestHelpers (deleteSpaces)
import Trees.Print (display)
import Trees.Parsing ()
import Trees.Types (SynTree, BinOp, PropFormula)
import SynTreeSpec (validBoundsSynTree)
import Formula.Parsing (Parse(parser))
import Control.OutputCapable.Blocks (LangM)
import Data.Maybe (isJust)
import Control.Monad.Identity (Identity(runIdentity))
import Control.OutputCapable.Blocks.Generic (evalLangM)

validBoundsSubTree :: Gen SubTreeConfig
validBoundsSubTree = do
    allowSameSubTree <- elements [True,False]
    syntaxTreeConfig@SynTreeConfig {..} <- validBoundsSynTree `suchThat` ((4<=) . minNodes)
    subTreeAmount <- choose (2, minNodes - maxLeavesForNodes minNodes)
    return $ SubTreeConfig
      {
        syntaxTreeConfig
      , allowSameSubTree
      , subTreeAmount
      , extraText = Nothing
      , printSolution = False
      , offerUnicodeInput = False
      }

spec :: Spec
spec = do
  describe "config" $ do
      it "default config should pass config check" $
        isJust $ runIdentity $ evalLangM (checkSubTreeConfig defaultSubTreeConfig :: LangM Maybe)
      it "validBoundsSubTree should generate a valid config" $
        forAll validBoundsSubTree $ \subTreeConfig ->
          isJust $ runIdentity $ evalLangM (checkSubTreeConfig subTreeConfig :: LangM Maybe)
  describe "generateSubTreeInst" $ do
    it "parse should works well" $
      forAll validBoundsSubTree $ \subTreeConfig ->
        forAll (generateSubTreeInst subTreeConfig) $ \SubTreeInst{..} ->
          let
            correctTrees = allNotLeafSubTrees tree
          in
            all (\tree -> parse (parser @(SynTree BinOp Char)) "" (display tree) == Right tree) correctTrees
    it "correct formulas are stored" $
      forAll validBoundsSubTree $ \subTreeConfig ->
        forAll (generateSubTreeInst subTreeConfig) $ \SubTreeInst{..} ->
          let
            correctTrees' = allNotLeafSubTrees tree
          in
            correctTrees == correctTrees'
    it "it should generate not less Syntax Sub tree number it required as excepted" $
      forAll validBoundsSubTree $ \config@SubTreeConfig {..} ->
        forAll (generateSubTreeInst config) $ \SubTreeInst{..} ->
          fromIntegral (size correctTrees) >= subTreeAmount
    it "all subformulas are the sublist of the formula" $
      forAll validBoundsSubTree $ \config@SubTreeConfig {..} ->
        forAll (generateSubTreeInst config) $ \SubTreeInst{..} ->
          let
            correctFormulas = Data.Set.map display correctTrees
          in
            all (`isInfixOf` display tree) correctFormulas
    it "Converting correct subformulas Strings into formulas and parsing them again should yield the original" $
      forAll validBoundsSubTree $ \config ->
          forAll (generateSubTreeInst config) $ \SubTreeInst{..} ->
            let
              correctFormulas = Data.Set.map display correctTrees
              propFormulas = Data.Set.map
                (fromRight' . parse (parser @(PropFormula Char)) "")
                correctFormulas
              inputSet = Data.Set.map show propFormulas
            in
              inputSet == correctFormulas
    xit "The above should be true even when deleting spaces in the input" $
      forAll validBoundsSubTree $ \config ->
        forAll (generateSubTreeInst config) $ \SubTreeInst{..} ->
          let
            correctFormulas = Data.Set.map display correctTrees
            propFormulas = Data.Set.map
              (fromRight' . parse (parser @(PropFormula Char)) "" . deleteSpaces)
              correctFormulas
            inputSet = Data.Set.map show propFormulas
          in
            inputSet == correctFormulas
