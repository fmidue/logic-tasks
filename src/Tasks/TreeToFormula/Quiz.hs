{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.TreeToFormula.Quiz(
    generateTreeToFormulaInst,
    ) where


import Trees.Generate (genSynTree)
import Test.QuickCheck (Gen,)

import Tasks.TreeToFormula.Config (TreeToFormulaConfig(..), TreeToFormulaInst(..))
import Tasks.SynTree.Config (SynTreeConfig(binOpFrequencies))
import Trees.Print (transferToPicture, display)
import Trees.Types (BinOp(..))
import qualified Data.Map as Map (keys)




generateTreeToFormulaInst :: TreeToFormulaConfig -> Gen TreeToFormulaInst
generateTreeToFormulaInst TreeToFormulaConfig {..} = do
    tree <- genSynTree syntaxTreeConfig
    return $ TreeToFormulaInst
      { tree
      , latexImage = transferToPicture tree
      , correct = display tree
      , addExtraHintsOnSemanticEquivalence = extraHintsOnSemanticEquivalence
      , addExtraHintsOnAssociativity = extraHintsOnAssociativity
      , showArrowOperators = any (`elem` Map.keys (binOpFrequencies syntaxTreeConfig)) [Impl, BackImpl, Equi]
      , addText = extraText
      , showSolution = printSolution
      , unicodeAllowed = offerUnicodeInput
      }
