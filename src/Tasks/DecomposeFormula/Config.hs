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
import Control.Monad.Output (Language, OutputMonad, LangM, german, english)
import LogicTasks.Helpers (reject)

data DecomposeFormulaConfig = DecomposeFormulaConfig {
      syntaxTreeConfig :: SynTreeConfig
    , extraHintsOnAssociativity :: Bool
    , extraText :: Maybe (Map Language String)
    , printSolution :: Bool
    }
    deriving (Typeable, Generic, Show)

defaultDecomposeFormulaConfig :: DecomposeFormulaConfig
defaultDecomposeFormulaConfig = DecomposeFormulaConfig
    { syntaxTreeConfig = defaultSynTreeConfig { allowArrowOperators = True }
    , extraHintsOnAssociativity = True
    , extraText = Nothing
    , printSolution = True
    }



checkDecomposeFormulaConfig :: OutputMonad m => DecomposeFormulaConfig -> LangM m
checkDecomposeFormulaConfig config@DecomposeFormulaConfig{..} =
  checkSynTreeConfig syntaxTreeConfig *> checkAdditionalConfig config

checkAdditionalConfig :: OutputMonad m => DecomposeFormulaConfig -> LangM m
checkAdditionalConfig DecomposeFormulaConfig {syntaxTreeConfig=SynTreeConfig {..}}
    | minUniqueBinOperators < 1 = reject $ do
        english "There should be a positive number of (unique) operators."
        german "Es sollte eine positive Anzahl an (unterschiedlichen) Operatoren geben."
    | minNodes < 7 = reject $ do
        english "Minimum number of nodes restricts the number of possible subtrees too much."
        german "Minimale Anzahl an Knoten schränkt die Anzahl der möglichen Teilbäume zu stark ein."
    | otherwise = pure ()

data DecomposeFormulaInst = DecomposeFormulaInst
               { tree :: SynTree BinOp Char
               , addExtraHintsOnAssociativity :: Bool
               , addText :: Maybe (Map Language String)
               , showSolution :: Bool
               }
               deriving (Show, Typeable, Generic)

