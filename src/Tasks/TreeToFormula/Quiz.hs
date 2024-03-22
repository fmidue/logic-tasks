{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.TreeToFormula.Quiz(
    generateTreeToFormulaInst,
    ) where


import Trees.Generate (genSynTree)
import Test.QuickCheck (Gen,)

import Tasks.TreeToFormula.Config (TreeToFormulaConfig(..), TreeToFormulaInst(..))
import Tasks.SynTree.Config (SynTreeConfig(..))
import Trees.Print (transferToPicture, display)




generateTreeToFormulaInst :: TreeToFormulaConfig -> Gen TreeToFormulaInst
generateTreeToFormulaInst TreeToFormulaConfig {..} = do
    tree <- genSynTree syntaxTreeConfig
    return $ TreeToFormulaInst
      { tree
      , latexImage = transferToPicture tree
      , correct = display tree
      , addExtraHintsOnSemanticEquivalence = extraHintsOnSemanticEquivalence
      , showArrowOperators = allowArrowOperators syntaxTreeConfig
      , addText = extraText
      , showSolution = printSolution
      }
