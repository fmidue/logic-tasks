{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module ComposeFormulaSpec where

import Test.Hspec
import Tasks.ComposeFormula.Config (
  ComposeFormulaConfig(..),
  TreeDisplayMode (FormulaDisplay),
  checkComposeFormulaConfig,
  defaultComposeFormulaConfig,
  ComposeFormulaInst(..))
import Test.QuickCheck
import SynTreeSpec (validBoundsSynTree)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Control.Monad.Output (LangM)
import Data.Maybe (isJust)
import Control.Monad.Identity (Identity(runIdentity))
import Control.Monad.Output.Generic (evalLangM)
import Tasks.ComposeFormula.Quiz (generateComposeFormulaInst)

validBoundsComposeFormula :: Gen ComposeFormulaConfig
validBoundsComposeFormula = do
  syntaxTreeConfig <- validBoundsSynTree `suchThat` \SynTreeConfig{..} ->
    minUniqueBinOperators >= 1 && minNodes >= 2 * minUniqueBinOperators + 3
  displayModeL <- elements [minBound..maxBound :: TreeDisplayMode]
  displayModeR <- elements [minBound..maxBound :: TreeDisplayMode]
  return ComposeFormulaConfig {
    syntaxTreeConfig,
    treeDisplayModes = (displayModeL, displayModeR),
    extraHintsOnAssociativity = False,
    extraText = Nothing,
    printSolution = False
  }

spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      isJust $ runIdentity $ evalLangM (checkComposeFormulaConfig defaultComposeFormulaConfig :: LangM Maybe)
    it "validBoundsComposeFormula should generate a valid config" $
      forAll validBoundsComposeFormula $ \composeFormulaConfig ->
        isJust $ runIdentity $ evalLangM (checkComposeFormulaConfig composeFormulaConfig :: LangM Maybe)
  describe "generateComposeFormulaInst" $ do
    it "should generate an instance with different subtrees" $
      forAll validBoundsComposeFormula $ \composeFormulaConfig ->
        forAll (generateComposeFormulaInst composeFormulaConfig) $ \ComposeFormulaInst{..} ->
          leftTree /= rightTree
    it "leftTreeImage and rightTreeImage has the right value" $
      forAll validBoundsComposeFormula $ \composeFormulaConfig@ComposeFormulaConfig{..} ->
        forAll (generateComposeFormulaInst composeFormulaConfig) $ \ComposeFormulaInst{..} ->
          fst treeDisplayModes == FormulaDisplay || isJust leftTreeImage &&
          snd treeDisplayModes == FormulaDisplay || isJust rightTreeImage
