{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Tasks.DecomposeFormula.Config (
    DecomposeFormulaConfig (..),
    DecomposeFormulaInst (..),
    defaultDecomposeFormulaConfig,
    checkDecomposeFormulaConfig
    ) where

import Tasks.SynTree.Config (SynTreeConfig(..), defaultSynTreeConfig, checkSynTreeConfig)
import Data.Map (Map)
import Trees.Types (SynTree(..), BinOp(..))
import Data.Typeable
import GHC.Generics
import Control.Monad.Output (Language, OutputMonad, LangM)

data DecomposeFormulaConfig = DecomposeFormulaConfig {
      syntaxTreeConfig :: SynTreeConfig
    , extraHintsOnAssociativity :: Bool
    , extraText :: Maybe (Map Language String)
    , printSolution :: Bool
    }
    deriving (Typeable, Generic)

defaultDecomposeFormulaConfig :: DecomposeFormulaConfig
defaultDecomposeFormulaConfig = DecomposeFormulaConfig
    { syntaxTreeConfig = defaultSynTreeConfig
    , extraHintsOnAssociativity = True
    , extraText = Nothing
    , printSolution = True
    }



checkDecomposeFormulaConfig :: OutputMonad m => DecomposeFormulaConfig -> LangM m
checkDecomposeFormulaConfig DecomposeFormulaConfig {..} =
    checkSynTreeConfig syntaxTreeConfig


data DecomposeFormulaInst = DecomposeFormulaInst
               { tree :: SynTree BinOp Char
               , addExtraHintsOnAssociativity :: Bool
               , addText :: Maybe (Map Language String)
               , showSolution :: Bool
               }
               deriving (Show, Typeable, Generic)

