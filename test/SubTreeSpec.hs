{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module SubTreeSpec (spec) where

import Test.Hspec ( describe, it, Spec )
import Test.QuickCheck (Gen, choose, forAll, elements, suchThat)
import Text.Parsec (parse)
import Data.Either (fromRight)
import Data.List (intercalate)
import Data.List.Extra (isInfixOf )
import Data.Set (size, toList, map)

import Tasks.SubTree.Config (SubTreeConfig(..), SubTreeInst(..))
import Tasks.SubTree.Quiz (generateSubTreeInst, feedback)
import Trees.Types (SynTree(..), BinOp(..), PropFormula(..))
import Trees.Helpers (maxLeavesForNodes)
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
                forAll (generateSubTreeInst subTreeConfig) $ \SubTreeInst{..} -> subTreeStringParse (displaySubTrees (toList correctTrees)) == Right correctTrees
        it "parse should works well" $
            forAll validBoundsSubTree $ \subTreeConfig ->
                forAll (generateSubTreeInst subTreeConfig) $ \SubTreeInst{..} -> subFormulasStringParse (deleteSpaces (displaySubTrees (toList correctTrees))) == Right (Data.Set.map deleteSpaces correctFormulas)
        it "it should generate not less Syntax Sub tree number it required as excepted" $
            forAll validBoundsSubTree $ \sTconfig@SubTreeConfig {..} ->
                forAll (generateSubTreeInst sTconfig) $ \SubTreeInst{..} -> fromIntegral (size correctFormulas) >= minSubTrees
        it "all subformulas is the sublist of formula" $
            forAll validBoundsSubTree $ \sTconfig@SubTreeConfig {..} ->
                forAll (generateSubTreeInst sTconfig) $ \SubTreeInst{..} -> let correctFormulas' = toList correctFormulas in all (`isInfixOf` formula) correctFormulas'
        it "the correct store in Inst should be accept by feedback" $
            forAll validBoundsSubTree $ \subTreeConfig ->
                forAll (generateSubTreeInst subTreeConfig) $ \subConfig@SubTreeInst{..} ->  feedback subConfig $ Prelude.map (\s -> fromRight (Atomic ' ') (parse parsePropForm "" s)) $ toList correctFormulas
        it "the correct store in Inst should be accept by feedback, even without spaces" $
            forAll validBoundsSubTree $ \subTreeConfig ->
                forAll (generateSubTreeInst subTreeConfig) $ \subConfig@SubTreeInst{..} ->  feedback subConfig $ Prelude.map (\s -> fromRight (Atomic ' ') (parse parsePropForm "" $ deleteSpaces s)) $ toList correctFormulas

displaySubTrees :: [SynTree BinOp Char] -> String
displaySubTrees trees = "{" ++ showTrees trees ++ "}"

showTrees :: [SynTree BinOp Char] -> String
showTrees synTreeList = intercalate ", " (Prelude.map display synTreeList)
