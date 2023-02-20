{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module SubTreeSpec (spec) where

import Test.Hspec (describe, it, Spec)
import Test.QuickCheck (Gen, choose, forAll, elements, suchThat)
import Text.Parsec (parse)
import Data.Either (fromRight)
import Data.List (intercalate)
import Data.List.Extra (isInfixOf )
import Data.Set (fromList, size, toList, map)

import Tasks.SubTree.Config (SubTreeConfig(..), SubTreeInst(..))
import Tasks.SubTree.Quiz (generateSubTreeInst)
import Trees.Types (SynTree(..), BinOp(..), PropFormula(..))
import Trees.Helpers (allNotLeafSubTrees, maxLeavesForNodes)
import Tasks.SynTree.Config (SynTreeConfig(..),)
import TestHelpers (deleteSpaces)
import Trees.Print (display)
import Trees.Parsing (parsePropForm)
import Tasks.SubTree.Parsing (subFormulasStringParse, subTreeStringParse)
import SynTreeSpec (validBoundsSynTree)

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
                    subTreeStringParse (displaySubTrees $ toList correctTrees) == Right correctTrees
        it "parse should works well" $
            forAll validBoundsSubTree $ \subTreeConfig ->
                forAll (generateSubTreeInst subTreeConfig) $ \SubTreeInst{..} ->
                  let
                    correctTrees = allNotLeafSubTrees tree
                  in
                    subFormulasStringParse (deleteSpaces $ displaySubTrees $ toList correctTrees)
                      == Right (Data.Set.map deleteSpaces correctFormulas)
        it "it should generate not less Syntax Sub tree number it required as excepted" $
            forAll validBoundsSubTree $ \config@SubTreeConfig {..} ->
                forAll (generateSubTreeInst config) $ \SubTreeInst{..} ->
                  fromIntegral (size correctFormulas) >= minSubTrees
        it "all subformulae are the sublist of the formula" $
            forAll validBoundsSubTree $ \config@SubTreeConfig {..} ->
                forAll (generateSubTreeInst config) $ \SubTreeInst{..} ->
                  let
                    correctFormulas' = toList correctFormulas
                  in
                    all (`isInfixOf` display tree) correctFormulas'
        it "Converting correct subformulae Strings into formulae and parsing them again should yield the original" $
            forAll validBoundsSubTree $ \config ->
                forAll (generateSubTreeInst config) $ \SubTreeInst{..} ->
                  let
                    propFormulae = Prelude.map
                      (fromRight (Atomic ' ') . parse parsePropForm "")
                      (toList correctFormulas)
                    inputSet = fromList (Prelude.map show propFormulae)
                  in
                    inputSet == correctFormulas
        it "The above should be true even when deleting spaces in the input" $
            forAll validBoundsSubTree $ \config ->
                forAll (generateSubTreeInst config) $ \SubTreeInst{..} ->
                  let
                    propFormulae = Prelude.map
                      (fromRight (Atomic ' ') . parse parsePropForm "" . deleteSpaces)
                      (toList correctFormulas)
                    inputSet = fromList (Prelude.map show propFormulae)
                  in
                    inputSet == correctFormulas



displaySubTrees :: [SynTree BinOp Char] -> String
displaySubTrees trees = "{" ++ showTrees trees ++ "}"

showTrees :: [SynTree BinOp Char] -> String
showTrees synTreeList = intercalate ", " (Prelude.map display synTreeList)
