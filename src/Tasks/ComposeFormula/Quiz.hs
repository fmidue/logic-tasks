{-# LANGUAGE RecordWildCards #-}

module Tasks.ComposeFormula.Quiz(
    generateComposeFormulaInst,
    ) where


import Trees.Generate (genSynTree)
import Test.QuickCheck (Gen, suchThat,)

import Tasks.ComposeFormula.Config (ComposeFormulaConfig(..), ComposeFormulaInst(..), TreeDisplayMode (FormulaDisplay))
import Tasks.SynTree.Config (SynTreeConfig(..))
import Trees.Helpers (binOp, bothKids)
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
    let (leftTree, rightTree) = bothKids tree
    return $ ComposeFormulaInst
      { operator = fromJust $ binOp tree
      , leftTree = leftTree
      , rightTree = rightTree
      , leftTreeImage = if fst treeDisplayModes == FormulaDisplay then Nothing else Just $ transferToPicture leftTree
      , rightTreeImage = if snd treeDisplayModes == FormulaDisplay then Nothing else Just $ transferToPicture rightTree
      , addExtraHintsOnAssociativity = extraHintsOnAssociativity
      , addText = extraText
      , showSolution = printSolution
      }
