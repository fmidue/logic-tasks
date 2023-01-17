{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module SynTreeSpec (spec, validBoundsSynTree) where

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, choose, elements, forAll, sublistOf, suchThat, arbitrary)
import Data.List.Extra ( nubOrd, isInfixOf )
import Data.Either (isRight)

import TestHelpers (deleteSpaces)
import Trees.Print (display)
import Trees.Parsing (formulaParse)
import Tasks.SynTree.Config (SynTreeConfig (..), SynTreeInst (..))
import Trees.Helpers (collectLeaves, treeDepth, treeNodes, maxLeavesForNodes, maxNodesForDepth, minDepthForNodes)
import Tasks.SynTree.Quiz (generateSynTreeInst, feedback)

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
  maxDepth <- choose (minDepthForNodes minNodes', 1 + result * (maxConsecutiveNegations + 1) + min maxConsecutiveNegations rest)
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
        maxConsecutiveNegations
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
        maxConsecutiveNegations
      }



spec :: Spec
spec = do
  describe "feedback" $
    it "rejects nonsense" $
      forAll validBoundsSynTree $ \sTConfig ->
        forAll (generateSynTreeInst sTConfig) $ \sTInst@SynTreeInst{..} ->
          forAll arbitrary $ \pFormula -> not $ feedback sTInst pFormula
  describe "genSyntaxTree" $ do
    it "should generate a random SyntaxTree from the given parament and can be parsed by formulaParse" $
      forAll validBoundsSynTree $ \sTConfig ->
        forAll (generateSynTreeInst sTConfig) $ \SynTreeInst{..} -> isRight $ formulaParse correct
    it "should generate a random SyntaxTree from the given parament and can be parsed by formulaParse, even without spaces" $
      forAll validBoundsSynTree $ \sTConfig ->
        forAll (generateSynTreeInst sTConfig) $ \SynTreeInst{..} -> isRight $ formulaParse $ deleteSpaces correct
    it "should generate a random SyntaxTree from the given parament and in the node area" $
      forAll validBoundsSynTree $ \sTConfig@SynTreeConfig {..} ->
        forAll (generateSynTreeInst sTConfig) $ \SynTreeInst{..} -> treeNodes instSynTree >= minNodes && treeNodes instSynTree <= maxNodes
    it "should generate a random SyntaxTree from the given parament and not deeper than the maxDepth" $
      forAll validBoundsSynTree $ \sTConfig@SynTreeConfig {..} ->
        forAll (generateSynTreeInst sTConfig) $ \SynTreeInst{..} -> treeDepth instSynTree <= maxDepth
    it "should generate a random SyntaxTree from the given parament and use as many chars as it must use" $
      forAll validBoundsSynTree $ \sTConfig@SynTreeConfig {..} ->
        forAll (generateSynTreeInst sTConfig) $ \SynTreeInst{..} -> fromIntegral (length (nubOrd (collectLeaves instSynTree))) >= atLeastOccurring
    it "should generate a random SyntaxTree with limited ConsecutiveNegations" $
      forAll validBoundsSynTree $ \sTConfig@SynTreeConfig {..} ->
        forAll (generateSynTreeInst sTConfig) $ \SynTreeInst{..} ->  not (replicate (fromIntegral maxConsecutiveNegations + 1) '~' `isInfixOf` deleteSpaces (display instSynTree))
