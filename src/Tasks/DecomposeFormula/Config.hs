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
import qualified Data.Map as Map (fromList, findWithDefault)
import Trees.Types (SynTree(..), BinOp(..))
import Data.Typeable
import GHC.Generics
import Control.OutputCapable.Blocks (LangM, Language, OutputCapable, german, english)
import LogicTasks.Helpers (reject)

data DecomposeFormulaConfig = DecomposeFormulaConfig {
      syntaxTreeConfig :: SynTreeConfig
    , extraText :: Maybe (Map Language String)
    , printSolution :: Bool
    , offerUnicodeInput :: Bool
    }
    deriving (Typeable, Generic, Show)

defaultDecomposeFormulaConfig :: DecomposeFormulaConfig
defaultDecomposeFormulaConfig = DecomposeFormulaConfig
    { syntaxTreeConfig = defaultSynTreeConfig
      { binOpFrequencies = Map.fromList
        [ (And, 1)
        , (Or, 1)
        , (Impl, 1)
        , (BackImpl, 1)
        , (Equi, 1)
        ]
      }
    , extraText = Nothing
    , printSolution = True
    , offerUnicodeInput = False
    }



checkDecomposeFormulaConfig :: OutputCapable m => DecomposeFormulaConfig -> LangM m
checkDecomposeFormulaConfig config@DecomposeFormulaConfig{..} =
  checkSynTreeConfig syntaxTreeConfig *> checkAdditionalConfig config

checkAdditionalConfig :: OutputCapable m => DecomposeFormulaConfig -> LangM m
checkAdditionalConfig DecomposeFormulaConfig {syntaxTreeConfig=SynTreeConfig {..}}
    | minUniqueBinOperators < 1 = reject $ do
        english "There should be a positive number of (unique) operators."
        german "Es sollte eine positive Anzahl an (unterschiedlichen) Operatoren geben."
    | minNodes < 7 = reject $ do
        english "Minimum number of nodes restricts the number of possible subtrees too much."
        german "Minimale Anzahl an Knoten schränkt die Anzahl der möglichen Teilbäume zu stark ein."
    | all ((== 0) . freq) [And, Or, Equi] = reject $ do
        english "At least one of the following operators must have a frequency greater than 0: And, Or, Equi"
        german "Mindestens einer der folgenden Operatoren muss eine Frequenz größer als 0 besitzen: And, Or, Equi"
    | otherwise = pure ()
    where freq op = Map.findWithDefault (0 :: Int) op binOpFrequencies

data DecomposeFormulaInst = DecomposeFormulaInst
               { tree :: SynTree BinOp Char
               , addText :: Maybe (Map Language String)
               , showSolution :: Bool
               , unicodeAllowed :: Bool
               }
               deriving (Show, Typeable, Generic)

