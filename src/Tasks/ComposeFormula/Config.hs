{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Tasks.ComposeFormula.Config (
    ComposeFormulaConfig (..),
    ComposeFormulaInst (..),
    defaultComposeFormulaConfig,
    checkComposeFormulaConfig,
    TreeDisplayMode(..)
    ) where

import Tasks.SynTree.Config (SynTreeConfig(..), defaultSynTreeConfig, checkSynTreeConfig)
import Data.Map (Map)
import Trees.Types (SynTree(..), BinOp(..))
import Data.Typeable
import GHC.Generics
import Control.Monad.Output (Language, OutputMonad, LangM, english, german)
import LogicTasks.Helpers (reject)

data TreeDisplayMode = FormulaDisplay | TreeDisplay deriving (Show,Eq, Enum, Bounded)

data ComposeFormulaConfig = ComposeFormulaConfig {
      syntaxTreeConfig :: SynTreeConfig
    , treeDisplayModes :: (TreeDisplayMode, TreeDisplayMode)
    , extraHintsOnAssociativity :: Bool
    , extraText :: Maybe (Map Language String)
    , printSolution :: Bool
    }
    deriving (Typeable, Generic, Show)

defaultComposeFormulaConfig :: ComposeFormulaConfig
defaultComposeFormulaConfig = ComposeFormulaConfig
    { syntaxTreeConfig = defaultSynTreeConfig { allowArrowOperators = True }
    , treeDisplayModes = (TreeDisplay, TreeDisplay)
    , extraHintsOnAssociativity = True
    , extraText = Nothing
    , printSolution = False
    }



checkComposeFormulaConfig :: OutputMonad m => ComposeFormulaConfig -> LangM m
checkComposeFormulaConfig config@ComposeFormulaConfig {..} =
    checkSynTreeConfig syntaxTreeConfig *> checkAdditionalConfig config

checkAdditionalConfig :: OutputMonad m => ComposeFormulaConfig -> LangM m
checkAdditionalConfig ComposeFormulaConfig {syntaxTreeConfig=SynTreeConfig {..}}
    | minUniqueBinOperators < 1 = reject $ do
        english "There should be a positive number of (unique) operators."
        german "Es sollte eine positive Anzahl an (unterschiedlichen) Operatoren geben."
    | otherwise = pure ()


data ComposeFormulaInst = ComposeFormulaInst
               { operator :: BinOp
               , leftTree :: SynTree BinOp Char
               , rightTree :: SynTree BinOp Char
               , leftTreeImage :: Maybe String
               , rightTreeImage :: Maybe String
               , addExtraHintsOnAssociativity :: Bool
               , addText :: Maybe (Map Language String)
               , showSolution :: Bool
               }
               deriving (Show, Typeable, Generic)

