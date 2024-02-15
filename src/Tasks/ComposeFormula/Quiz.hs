{-# LANGUAGE RecordWildCards #-}

module Tasks.ComposeFormula.Quiz(
    generateComposeFormulaInst,
    ) where


import Trees.Generate (genSynTree)
import Test.QuickCheck (Gen, suchThat,)

import Tasks.ComposeFormula.Config (ComposeFormulaConfig(..), ComposeFormulaInst(..))
import Tasks.SynTree.Config (SynTreeConfig(..))
import Trees.Helpers (binOp, subTrees)
import Data.Maybe (fromJust, isJust)
import Trees.Print (transferToPicture)




generateComposeFormulaInst :: ComposeFormulaConfig -> Gen ComposeFormulaInst
generateComposeFormulaInst ComposeFormulaConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} = do
    tree <- genSynTree
        (minNodes, maxNodes)
        maxDepth
        usedLiterals
        atLeastOccurring
        allowArrowOperators
        maxConsecutiveNegations
        minUniqueBinOperators
          `suchThat` \synTree -> isJust $ binOp synTree
    let subtrees = subTrees tree
    return $ ComposeFormulaInst
      { operator = fromJust $ binOp tree
      , leftTree = head subtrees
      , rightTree = head $ tail subtrees
      , leftTreeDisplayMode = fst treeDisplayModes
      , rightTreeDisplayMode = snd treeDisplayModes
      , leftTreeImage = transferToPicture $ head subtrees
      , rightTreeImage = transferToPicture $ head $ tail subtrees
      , addExtraHintsOnSemanticEquivalence = extraHintsOnSemanticEquivalence
      , addText = extraText
      , showSolution = printSolution
      }
