{-# LANGUAGE DeriveDataTypeable #-}
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

-- jscpd:ignore-start
import Tasks.SynTree.Config (SynTreeConfig(..), defaultSynTreeConfig, checkSynTreeConfig)
import Data.Data (Data)
import Data.Map (Map)
import qualified Data.Map as Map (fromList)
import Trees.Types (SynTree(..), BinOp(..))
import Data.Typeable
import GHC.Generics
import Control.OutputCapable.Blocks (LangM, Language, OutputCapable, english, german)
import LogicTasks.Helpers (reject)
-- jscpd:ignore-end

data TreeDisplayMode = FormulaDisplay | TreeDisplay deriving (Show,Eq, Enum, Bounded)

data ComposeFormulaConfig = ComposeFormulaConfig {
      syntaxTreeConfig :: SynTreeConfig
    , treeDisplayModes :: (TreeDisplayMode, TreeDisplayMode)
    , extraHintsOnAssociativity :: Bool
    , extraText :: Maybe (Map Language String)
    , printSolution :: Bool
    , offerUnicodeInput :: Bool
    }
    deriving (Typeable, Generic, Show)

defaultComposeFormulaConfig :: ComposeFormulaConfig
defaultComposeFormulaConfig = ComposeFormulaConfig
    { syntaxTreeConfig = defaultSynTreeConfig
      { binOpFrequencies = Map.fromList
        [ (And, 1)
        , (Or, 1)
        , (Impl, 1)
        , (BackImpl, 1)
        , (Equi, 1)
        ]
      }
    , treeDisplayModes = (TreeDisplay, TreeDisplay)
    , extraHintsOnAssociativity = True
    , extraText = Nothing
    , printSolution = False
    , offerUnicodeInput = False
    }



checkComposeFormulaConfig :: OutputCapable m => ComposeFormulaConfig -> LangM m
checkComposeFormulaConfig config@ComposeFormulaConfig {..} =
    checkSynTreeConfig syntaxTreeConfig *> checkAdditionalConfig config

checkAdditionalConfig :: OutputCapable m => ComposeFormulaConfig -> LangM m
checkAdditionalConfig ComposeFormulaConfig {syntaxTreeConfig=SynTreeConfig {..}}
    | minUniqueBinOperators < 1 = reject $ do
        english "There should be a positive number of (unique) operators."
        german "Es sollte eine positive Anzahl an (unterschiedlichen) Operatoren geben."
    | minNodes < 7 = reject $ do
        english "Minimum number of nodes restricts the number of possible subtrees too much."
        german "Minimale Anzahl an Knoten schränkt die Anzahl der möglichen Teilbäume zu stark ein."
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
               , unicodeAllowed :: Bool
               }
               deriving (Data, Show, Typeable, Generic)

