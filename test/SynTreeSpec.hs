{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module SynTreeSpec (spec, validBoundsSynTree) where

import Test.Hspec (Spec, describe, it, xit)
import Test.QuickCheck (Gen, choose, elements, forAll, sublistOf, suchThat)
import Data.List.Extra (nubOrd, isInfixOf)

import TestHelpers (deleteSpaces)
import Trees.Print (display)
import Trees.Parsing (formulaParse)
import Tasks.SynTree.Config (SynTreeConfig (..), SynTreeInst (..), defaultSynTreeConfig)
import Trees.Helpers (
  collectLeaves,
  treeDepth,
  treeNodes,
  maxLeavesForNodes,
  maxNodesForDepth,
  minDepthForNodes,
  numOfUniqueBinOpsInSynTree)
import Tasks.SynTree.Quiz (generateSynTreeInst)
import SAT.MiniSat hiding (Formula(Not))
import qualified SAT.MiniSat as Sat (Formula(Not))
import Trees.Types (SynTree(..), BinOp(..))
import LogicTasks.Formula (ToSAT(convert), isSemanticEqual)
import Trees.Generate (genSynTree)

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
        extraHintsOnSemanticEquivalence = False,
        minUniqueBinOperators = 0
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
        extraHintsOnSemanticEquivalence = False,
        minUniqueBinOperators = 0
      }



spec :: Spec
spec = do
  describe "feedback" $
    it "rejects nonsense" $
      forAll validBoundsSynTree $ \config ->
        forAll (generateSynTreeInst config) $ \SynTreeInst{..} -> formulaParse (tail correct) /= Right tree
  describe "numOfUniqueBinOpsInSynTree" $ do
        it "should return 0 if there is only a leaf" $
            numOfUniqueBinOpsInSynTree (Leaf 'a') == 0
        it "should return 1 if there is only one operator" $
            numOfUniqueBinOpsInSynTree (Binary Or (Leaf 'a') (Leaf 'b')) == 1
        it "should return 1 if there are two operators of same kind" $
            numOfUniqueBinOpsInSynTree (Binary Or (Leaf 'a') (Not (Binary Or (Leaf 'a') (Leaf 'c')))) == 1
        it "should return 2 if there are two unique operators" $
            let subtree = Binary And (Leaf 'a') in
            numOfUniqueBinOpsInSynTree (Binary Or (Leaf 'a') (Not (subtree (subtree (Leaf 'c'))))) == 2
  describe "genSynTree" $ do
    it "should generate a random SyntaxTree that satisfies the required amount of unique binary operators" $
      forAll validBoundsSynTree $ \SynTreeConfig {..} ->
        forAll (genSynTree
                    (minNodes, maxNodes)
                    maxDepth
                    usedLiterals
                    atLeastOccurring
                    allowArrowOperators
                    maxConsecutiveNegations
                    minUniqueBinOperators
                  ) $ \synTree -> numOfUniqueBinOpsInSynTree synTree >= minUniqueBinOperators
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
  describe "ToSAT instance" $ do
    it "should correctly convert Leaf" $
      convert @(SynTree BinOp Char) (Leaf 'A') == Var 'A'
    it "should correctly convert Not" $ do
      convert @(SynTree BinOp Char) (Not (Leaf 'A')) == Sat.Not (Var 'A') &&
        convert (Not (Binary And (Binary Impl (Leaf 'A') (Not (Leaf 'B'))) (Leaf 'C')))
          == Sat.Not((Var 'A' :->: Sat.Not (Var 'B')) :&&: Var 'C')
    it "should correctly convert Binary" $
      let orTree = Binary Or (Leaf 'C') (Leaf 'D') in
      convert (Binary And (Leaf 'A') (Leaf 'B')) == (Var 'A' :&&: Var 'B') &&
      convert orTree == (Var 'C' :||: Var 'D') &&
      convert (Binary Impl (Leaf 'A') (Leaf 'B')) == (Var 'A' :->: Var 'B') &&
      convert (Binary Equi (Leaf 'A') (Leaf 'B')) == (Var 'A' :<->: Var 'B') &&
      convert (Binary And (Binary Impl (Leaf 'A') (Not (Leaf 'B'))) (Binary Equi orTree (Leaf 'E')))
        == (Var 'A' :->: Sat.Not (Var 'B')) :&&: ((Var 'C' :||: Var 'D') :<->: Var 'E')

  describe "semantic equivalence of syntax trees (isSemanticEqual)" $  do
    it "a syntax tree's formula is semantically equivalent to itself" $
      forAll (generateSynTreeInst defaultSynTreeConfig) $ \(SynTreeInst tree _ _ _ _) ->
        isSemanticEqual tree tree
    it "a syntax tree's formula is semantically equivalent to itself with associativity applied" $ do
      isSemanticEqual
        ((Leaf 'A' `treeAnd` Leaf 'B') `treeAnd` Leaf 'C')
        (Leaf 'A' `treeAnd` (Leaf 'B' `treeAnd` Leaf 'C')) &&
        isSemanticEqual
          ((Leaf 'A' `treeOr` Leaf 'B') `treeOr` Leaf 'C')
          (Leaf 'A' `treeOr` (Leaf 'B' `treeOr` Leaf 'C'))
    it "a syntax tree's formula is semantically equivalent to itself with commutativity applied" $ do
      isSemanticEqual (Leaf 'A' `treeAnd` Leaf 'B') (Leaf 'B' `treeAnd` Leaf 'A') &&
        isSemanticEqual (Leaf 'A' `treeOr` Leaf 'B') (Leaf 'B' `treeOr` Leaf 'A') &&
          isSemanticEqual (Leaf 'A' `treeBiImpl` Leaf 'B') (Leaf 'B' `treeBiImpl` Leaf 'A')
    it "a syntax tree's formula is semantically equivalent to itself with distributivity applied" $ do
      isSemanticEqual
        ((Leaf 'A' `treeAnd` Leaf 'B') `treeOr` Leaf 'C')
        ((Leaf 'A' `treeOr` Leaf 'C') `treeAnd` (Leaf 'B' `treeOr` Leaf 'C')) &&
        isSemanticEqual
          ((Leaf 'A' `treeOr` Leaf 'B') `treeAnd` Leaf 'C')
          ((Leaf 'A' `treeAnd` Leaf 'C') `treeOr` (Leaf 'B' `treeAnd` Leaf 'C'))

-- shorthands
treeAnd, treeOr, treeBiImpl :: SynTree BinOp a -> SynTree BinOp a -> SynTree BinOp a
treeAnd = Binary And
treeOr = Binary Or
treeBiImpl = Binary Equi
