{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module DecomposeFormulaSpec where

import Test.Hspec
import Tasks.DecomposeFormula.Config (
  DecomposeFormulaConfig(..),
  checkDecomposeFormulaConfig,
  defaultDecomposeFormulaConfig,
  DecomposeFormulaInst(..))
import Test.QuickCheck
import SynTreeSpec (validBoundsSynTree)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Control.Monad.Output (LangM)
import Data.Maybe (isJust, fromJust)
import Control.Monad.Identity (Identity(runIdentity))
import Control.Monad.Output.Generic (evalLangM)
import Tasks.DecomposeFormula.Quiz (generateDecomposeFormulaInst)
import Trees.Helpers (bothKids, binOp)
import Trees.Types (SynTree(..))
import Trees.Print (display)

validBoundsDecomposeFormula :: Gen DecomposeFormulaConfig
validBoundsDecomposeFormula = do
  syntaxTreeConfig <- validBoundsSynTree `suchThat` \SynTreeConfig{..} ->
    minUniqueBinOperators >= 1 && minNodes > 6
  return DecomposeFormulaConfig {
    syntaxTreeConfig,
    extraHintsOnAssociativity = False,
    extraText = Nothing,
    printSolution = False
  }

spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      isJust $ runIdentity $ evalLangM (checkDecomposeFormulaConfig defaultDecomposeFormulaConfig :: LangM Maybe)
    it "validBoundsDecomposeFormula should generate a valid config" $
      forAll validBoundsDecomposeFormula $ \decomposeFormulaConfig ->
        isJust $ runIdentity $ evalLangM (checkDecomposeFormulaConfig decomposeFormulaConfig :: LangM Maybe)
  describe "generateDecomposeFormulaInst" $ do
    it "should generate an instance with different subtrees" $
      forAll validBoundsDecomposeFormula $ \decomposeFormulaConfig ->
        forAll (generateDecomposeFormulaInst decomposeFormulaConfig) $ \DecomposeFormulaInst{..} ->
          let (lk,rk) = bothKids tree
              rootOp = fromJust $ binOp tree
          in notElem (display (Binary rootOp lk rk)) [display (Binary rootOp rk lk), reverse (display (Binary rootOp lk rk))]
