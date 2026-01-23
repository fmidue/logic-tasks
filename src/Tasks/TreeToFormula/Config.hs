{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Tasks.TreeToFormula.Config (
    TreeToFormulaConfig (..),
    TreeToFormulaInst (..),
    defaultTreeToFormulaConfig,
    checkTreeToFormulaConfig,
    ) where

import Tasks.SynTree.Config (SynTreeConfig(..), defaultSynTreeConfig, checkSynTreeConfig)
import Trees.Types (SynTree(..), BinOp(..))
import GHC.Generics
import Control.OutputCapable.Blocks (LangM, OutputCapable, ExtraText (NoExtraText))

data TreeToFormulaConfig = TreeToFormulaConfig {
      syntaxTreeConfig :: SynTreeConfig
    , extraText :: ExtraText
    , printSolution :: Bool
    , offerUnicodeInput :: Bool
    }
  deriving (Generic, Show)

defaultTreeToFormulaConfig :: TreeToFormulaConfig
defaultTreeToFormulaConfig = TreeToFormulaConfig
    { syntaxTreeConfig = defaultSynTreeConfig
    , extraText = NoExtraText
    , printSolution = False
    , offerUnicodeInput = False
    }



checkTreeToFormulaConfig :: OutputCapable m => TreeToFormulaConfig -> LangM m
checkTreeToFormulaConfig TreeToFormulaConfig {..} =
    checkSynTreeConfig syntaxTreeConfig


data TreeToFormulaInst = TreeToFormulaInst {
                 tree :: SynTree BinOp Char
               , latexImage :: String
               , correct :: String
               , showArrowOperators :: Bool
               , addText :: ExtraText
               , showSolution :: Bool
               , unicodeAllowed :: Bool
               }
  deriving (Generic, Show)
