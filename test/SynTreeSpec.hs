{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module SynTreeSpec (spec, validBoundsSynTree) where

import Test.Hspec (Spec, describe, it, xit)
import Test.QuickCheck (Gen, choose, elements, forAll, sublistOf, suchThat)
import Data.List.Extra (nubOrd, isInfixOf)

import TestHelpers (deleteSpaces)
import Trees.Print (display)
import Trees.Parsing (formulaParse)
import Tasks.SynTree.Config (SynTreeConfig (..), SynTreeInst (..))
import Trees.Helpers (collectLeaves, treeDepth, treeNodes, maxLeavesForNodes, maxNodesForDepth, minDepthForNodes, binSynTreeToMiniSatFormula)
import Tasks.SynTree.Quiz (generateSynTreeInst)
import SAT.MiniSat hiding (Formula(Not))
import qualified SAT.MiniSat as Sat (Formula(Not))
import Trees.Types (SynTree(..), BinOp(..))

validBoundsSynTree :: Gen SynTreeConfig
validBoundsSynTree = do
  allowArrowOperators <- elements [True, False]
  maxConsecutiveNegations <- choose(0, 3)
  usedLiterals <- sublistOf ['A' .. 'Z'] `suchThat` (not . null)
  minNodes' <- choose (1, 20) `suchThat` \minNodes' -> maxConsecutiveNegations /= 0 || odd minNodes'
  maxNodes' <- choose (minNodes', 25) `suchThat` \maxNodes' -> maxConsecutiveNegations /= 0 || odd maxNodes'
  let maxNodes'' = maxNodes' - 1
      maxConsecutiveNegations' = maxConsecutiveNegations + 2
      (result, rest) = maxNodes'' `divMod` maxConsecutiveNegations'
  maxDepth <- choose
    (minDepthForNodes minNodes', 1 + result * (maxConsecutiveNegations + 1) + min maxConsecutiveNegations rest)
  let maxNodes = min maxNodes' (maxNodesForDepth maxDepth)
  useChars <- choose (1, maxLeavesForNodes maxNodes)
  let atLeastOccurring = min useChars (fromIntegral (length usedLiterals))
  return $
    SynTreeConfig
      { maxNodes,
        minNodes = max minNodes' (atLeastOccurring * 2 - 1),
        maxDepth,
        usedLiterals,
        atLeastOccurring,
        allowArrowOperators,
        maxConsecutiveNegations,
        extraText = Nothing,
        onlyAcceptExactFormula = False
      }

invalidBoundsSynTree :: Gen SynTreeConfig
invalidBoundsSynTree = do
  usedLiterals <- sublistOf ['A' .. 'Z']
  minNodes <- choose (2, 100)
  maxNodes <- choose (1, minNodes - 1)
  maxDepth <- choose (minDepthForNodes minNodes, maxNodes)
  maxConsecutiveNegations <- choose(1, 3)
  return $
    SynTreeConfig
      { maxNodes,
        minNodes,
        maxDepth,
        usedLiterals,
        atLeastOccurring = fromIntegral (length usedLiterals),
        allowArrowOperators = True,
        maxConsecutiveNegations,
        extraText = Nothing,
        onlyAcceptExactFormula = False
      }



spec :: Spec
spec = do
  describe "feedback" $
    it "rejects nonsense" $
      forAll validBoundsSynTree $ \config ->
        forAll (generateSynTreeInst config) $ \SynTreeInst{..} -> formulaParse (tail correct) /= Right tree
  describe "genSyntaxTree" $ do
    it "should generate a random SyntaxTree from the given parament and can be parsed by formulaParse" $
      forAll validBoundsSynTree $ \config ->
        forAll (generateSynTreeInst config) $ \SynTreeInst{..} -> formulaParse correct == Right tree
    xit ("should generate a random SyntaxTree from the given parament and can be parsed by formulaParse, " ++
        "even without spaces") $
      forAll validBoundsSynTree $ \config ->
        forAll (generateSynTreeInst config) $ \SynTreeInst{..} -> formulaParse (deleteSpaces correct) == Right tree
    it "should generate a random SyntaxTree from the given parament and in the node area" $
      forAll validBoundsSynTree $ \config@SynTreeConfig {..} ->
        forAll (generateSynTreeInst config) $ \SynTreeInst{..} ->
          treeNodes tree >= minNodes && treeNodes tree <= maxNodes
    it "should generate a random SyntaxTree from the given parament and not deeper than the maxDepth" $
      forAll validBoundsSynTree $ \config@SynTreeConfig {..} ->
        forAll (generateSynTreeInst config) $ \SynTreeInst{..} -> treeDepth tree <= maxDepth
    it "should generate a random SyntaxTree from the given parament and use as many chars as it must use" $
      forAll validBoundsSynTree $ \config@SynTreeConfig {..} ->
        forAll (generateSynTreeInst config) $ \SynTreeInst{..} ->
          fromIntegral (length (nubOrd (collectLeaves tree))) >= atLeastOccurring
    it "should generate a random SyntaxTree with limited ConsecutiveNegations" $
      forAll validBoundsSynTree $ \config@SynTreeConfig {..} ->
        forAll (generateSynTreeInst config) $ \SynTreeInst{..} ->
          not (replicate (fromIntegral maxConsecutiveNegations + 1) '~' `isInfixOf` deleteSpaces (display tree))
  describe "binSynTreeToMiniSatFormula" $ do
    it "should correctly convert Leaf" $
      binSynTreeToMiniSatFormula (Leaf 'A') == Var 'A'
    it "should correctly convert Not" $ do
      binSynTreeToMiniSatFormula (Not (Leaf 'A')) == Sat.Not (Var 'A') &&
        binSynTreeToMiniSatFormula (Not ((Binary And (Binary Impl (Leaf 'A') (Not (Leaf 'B'))) (Leaf 'C'))))
          == Sat.Not((Var 'A' :->: Sat.Not (Var 'B')) :&&: Var 'C')
    it "should correctly convert Binary" $
      binSynTreeToMiniSatFormula (Binary And (Leaf 'A') (Leaf 'B')) == (Var 'A' :&&: Var 'B') &&
      binSynTreeToMiniSatFormula (Binary Or (Leaf 'A') (Leaf 'B')) == (Var 'A' :||: Var 'B') &&
      binSynTreeToMiniSatFormula (Binary Impl (Leaf 'A') (Leaf 'B')) == (Var 'A' :->: Var 'B') &&
      binSynTreeToMiniSatFormula (Binary Equi (Leaf 'A') (Leaf 'B')) == (Var 'A' :<->: Var 'B') &&
      binSynTreeToMiniSatFormula (Binary And (Binary Impl (Leaf 'A') (Not (Leaf 'B'))) (Binary Equi (Binary Or (Leaf 'C') (Leaf 'D')) (Leaf 'E')))
        == (Var 'A' :->: Sat.Not (Var 'B')) :&&: ((Var 'C' :||: Var 'D') :<->: Var 'E')
