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
import Data.Typeable
import GHC.Generics
import Control.Monad.Output (Language, OutputMonad, LangM)

data TreeToFormulaConfig = TreeToFormulaConfig {
      syntaxTreeConfig :: SynTreeConfig
    , extraHintsOnSemanticEquivalence :: Bool
    , extraText :: Maybe (Map Language String)
    , printSolution :: Bool
    }
    deriving (Typeable, Generic)

defaultTreeToFormulaConfig :: TreeToFormulaConfig
defaultTreeToFormulaConfig = TreeToFormulaConfig
    { syntaxTreeConfig = defaultSynTreeConfig
    , extraHintsOnSemanticEquivalence = True
    , extraText = Nothing
    , printSolution = False
    }



checkTreeToFormulaConfig :: OutputMonad m => TreeToFormulaConfig -> LangM m
checkTreeToFormulaConfig TreeToFormulaConfig {..} =
    checkSynTreeConfig syntaxTreeConfig


data TreeToFormulaInst = TreeToFormulaInst {
                 tree :: SynTree BinOp Char
               , latexImage :: String
               , correct :: String
               , addExtraHintsOnSemanticEquivalence :: Bool
               , showArrowOperators :: Bool
               , addText :: Maybe (Map Language String)
               , showSolution :: Bool
               }
               deriving (Show, Typeable, Generic)

