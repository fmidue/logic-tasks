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
import Control.Monad.Output (Language, OutputMonad, LangM)

data TreeDisplayMode = FormulaDisplay | TreeDisplay deriving (Show,Eq)

data ComposeFormulaConfig = ComposeFormulaConfig {
      syntaxTreeConfig :: SynTreeConfig
    , treeDisplayModes :: (TreeDisplayMode, TreeDisplayMode)
    , extraHintsOnSemanticEquivalence :: Bool
    , extraText :: Maybe (Map Language String)
    , printSolution :: Bool
    }
    deriving (Typeable, Generic)

defaultComposeFormulaConfig :: ComposeFormulaConfig
defaultComposeFormulaConfig = ComposeFormulaConfig
    { syntaxTreeConfig = defaultSynTreeConfig
    , treeDisplayModes = (TreeDisplay, TreeDisplay)
    , extraHintsOnSemanticEquivalence = True
    , extraText = Nothing
    , printSolution = False
    }



checkComposeFormulaConfig :: OutputMonad m => ComposeFormulaConfig -> LangM m
checkComposeFormulaConfig ComposeFormulaConfig {..} =
    checkSynTreeConfig syntaxTreeConfig


data ComposeFormulaInst = ComposeFormulaInst
               { operator :: BinOp
               , leftTree :: SynTree BinOp Char
               , rightTree :: SynTree BinOp Char
               , leftTreeDisplayMode :: TreeDisplayMode
               , rightTreeDisplayMode :: TreeDisplayMode
               , leftTreeImage :: String
               , rightTreeImage :: String
               , addExtraHintsOnSemanticEquivalence :: Bool
               , addText :: Maybe (Map Language String)
               , showSolution :: Bool
               }
               deriving (Show, Typeable, Generic)
