{-# LANGUAGE RecordWildCards, NamedFieldPuns, TypeApplications #-}
module SubTreeSpec (spec) where

import Test.Hspec (describe, it, xit, Spec)
import Test.QuickCheck (Gen, choose, forAll, elements, suchThat)
import Text.Parsec (parse)
import Data.Either.Extra (fromRight')
import Data.List.Extra (isInfixOf )
import Data.Set (fromList, size, toList)

import Tasks.SubTree.Config (SubTreeConfig(..), SubTreeInst(..))
import Tasks.SubTree.Quiz (generateSubTreeInst)
import Trees.Helpers (allNotLeafSubTrees, maxLeavesForNodes)
import Tasks.SynTree.Config (SynTreeConfig(..),)
import TestHelpers (deleteSpaces)
import Trees.Print (display)
import Trees.Parsing ()
import Trees.Types (SynTree, BinOp, PropFormula)
import SynTreeSpec (validBoundsSynTree)
import Formula.Parsing (Parse(parser))

validBoundsSubTree :: Gen SubTreeConfig
validBoundsSubTree = do
    allowSameSubTree <- elements [True,False]
    syntaxTreeConfig@SynTreeConfig {..} <- validBoundsSynTree `suchThat` ((4<=) . minNodes)
    minSubTrees <- choose (2, minNodes - maxLeavesForNodes minNodes)
    return $ SubTreeConfig
      {
        syntaxTreeConfig
      , allowSameSubTree
      , minSubTrees
      }

invalidBoundsSubTree :: Gen SubTreeConfig
invalidBoundsSubTree = do
    allowSameSubTree <- elements [True,False]
    syntaxTreeConfig@SynTreeConfig {..} <- validBoundsSynTree
    minSubTrees <- choose (minNodes - maxLeavesForNodes minNodes + 1, 100)
    return $ SubTreeConfig
      {
        syntaxTreeConfig
      , allowSameSubTree
      , minSubTrees
      }

spec :: Spec
spec = do
  describe "generateSubTreeInst" $ do
    it "parse should works well" $
      forAll validBoundsSubTree $ \subTreeConfig ->
        forAll (generateSubTreeInst subTreeConfig) $ \SubTreeInst{..} ->
          let
            correctTrees = allNotLeafSubTrees tree
          in
            all (\tree -> parse (parser @(SynTree BinOp Char)) "" (display tree) == Right tree) $ toList correctTrees
    it "correct formulas are stored" $
      forAll validBoundsSubTree $ \subTreeConfig ->
        forAll (generateSubTreeInst subTreeConfig) $ \SubTreeInst{..} ->
          let
            correctTrees = allNotLeafSubTrees tree
          in
            fromList (map display $ toList correctTrees)
              == correctFormulas
    it "it should generate not less Syntax Sub tree number it required as excepted" $
      forAll validBoundsSubTree $ \config@SubTreeConfig {..} ->
        forAll (generateSubTreeInst config) $ \SubTreeInst{..} ->
          fromIntegral (size correctFormulas) >= minSubTrees
    it "all subformulas are the sublist of the formula" $
      forAll validBoundsSubTree $ \config@SubTreeConfig {..} ->
        forAll (generateSubTreeInst config) $ \SubTreeInst{..} ->
          let
            correctFormulas' = toList correctFormulas
          in
            all (`isInfixOf` display tree) correctFormulas'
    it "Converting correct subformulas Strings into formulas and parsing them again should yield the original" $
      forAll validBoundsSubTree $ \config ->
          forAll (generateSubTreeInst config) $ \SubTreeInst{..} ->
            let
              propFormulas = map
                (fromRight' . parse (parser @(PropFormula Char)) "")
                (toList correctFormulas)
              inputSet = fromList (map show propFormulas)
            in
              inputSet == correctFormulas
    xit "The above should be true even when deleting spaces in the input" $
      forAll validBoundsSubTree $ \config ->
        forAll (generateSubTreeInst config) $ \SubTreeInst{..} ->
          let
            propFormulas = map
              (fromRight' . parse (parser @(PropFormula Char)) "" . deleteSpaces)
              (toList correctFormulas)
            inputSet = fromList (map show propFormulas)
          in
            inputSet == correctFormulas
