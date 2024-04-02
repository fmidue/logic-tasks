{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tasks.DecomposeFormula.Quiz(
    generateDecomposeFormulaInst,
    ) where


import Trees.Generate (genSynTree)
import Test.QuickCheck (Gen, suchThat,)

import Tasks.DecomposeFormula.Config (DecomposeFormulaConfig(..), DecomposeFormulaInst(..))
import Trees.Helpers (binOp)
import Trees.Types (BinOp(Equi, Or, And))




generateDecomposeFormulaInst :: DecomposeFormulaConfig -> Gen DecomposeFormulaInst
generateDecomposeFormulaInst DecomposeFormulaConfig {..} = do
    tree <- genSynTree syntaxTreeConfig
          `suchThat` \synTree -> binOp synTree `elem` map Just [And, Or, Equi]
    return $ DecomposeFormulaInst
      { tree
      , addExtraHintsOnAssociativity = extraHintsOnAssociativity
      , addText = extraText
      , showSolution = printSolution
      }
