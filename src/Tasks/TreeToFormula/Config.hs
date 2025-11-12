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
import Data.Map (Map)
import Trees.Types (SynTree(..), BinOp(..))
import GHC.Generics
import Control.OutputCapable.Blocks (LangM, Language, OutputCapable)

data TreeToFormulaConfig = TreeToFormulaConfig {
      syntaxTreeConfig :: SynTreeConfig
    , extraText :: Maybe (Map Language String)
    , printSolution :: Bool
    , offerUnicodeInput :: Bool
    }
  deriving (Generic, Show)

defaultTreeToFormulaConfig :: TreeToFormulaConfig
defaultTreeToFormulaConfig = TreeToFormulaConfig
    { syntaxTreeConfig = defaultSynTreeConfig
    , extraText = Nothing
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
               , arrowOperatorsToShow :: [BinOp]
               , addText :: Maybe (Map Language String)
               , showSolution :: Bool
               , unicodeAllowed :: Bool
               }
  deriving (Generic, Show)
