{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Tasks.DecomposeFormula.Config (
    DecomposeFormulaConfig (..),
    DecomposeFormulaInst (..),
    defaultDecomposeFormulaConfig,
    checkDecomposeFormulaConfig
    ) where

import Prelude hiding (and, or)
import Tasks.SynTree.Config (SynTreeConfig(..), defaultSynTreeConfig, checkSynTreeConfig, OperatorFrequencies (..))
import Data.Map (Map)
import Trees.Types (SynTree(..), BinOp(..))
import Data.Typeable
import GHC.Generics
import Control.OutputCapable.Blocks (LangM, Language, OutputCapable, german, english)
import LogicTasks.Helpers (reject)

data DecomposeFormulaConfig = DecomposeFormulaConfig {
      syntaxTreeConfig :: SynTreeConfig
    , extraHintsOnAssociativity :: Bool
    , extraText :: Maybe (Map Language String)
    , printSolution :: Bool
    , offerUnicodeInput :: Bool
    }
    deriving (Typeable, Generic, Show)

defaultDecomposeFormulaConfig :: DecomposeFormulaConfig
defaultDecomposeFormulaConfig = DecomposeFormulaConfig
    { syntaxTreeConfig = defaultSynTreeConfig
      { operatorFrequencies = OperatorFrequencies
        { and = 1
        , or = 1
        , impl = 0
        , backImpl = 0
        , equi = 1
        , neg = 1
        }
      }
    , extraHintsOnAssociativity = True
    , extraText = Nothing
    , printSolution = True
    , offerUnicodeInput = False
    }



checkDecomposeFormulaConfig :: OutputCapable m => DecomposeFormulaConfig -> LangM m
checkDecomposeFormulaConfig config@DecomposeFormulaConfig{..} =
  checkSynTreeConfig syntaxTreeConfig *> checkAdditionalConfig config

checkAdditionalConfig :: OutputCapable m => DecomposeFormulaConfig -> LangM m
checkAdditionalConfig DecomposeFormulaConfig {syntaxTreeConfig=SynTreeConfig {operatorFrequencies=OperatorFrequencies{..},..}}
    | minUniqueBinOperators < 1 = reject $ do
        english "There should be a positive number of (unique) operators."
        german "Es sollte eine positive Anzahl an (unterschiedlichen) Operatoren geben."
    | minNodes < 7 = reject $ do
        english "Minimum number of nodes restricts the number of possible subtrees too much."
        german "Minimale Anzahl an Knoten schränkt die Anzahl der möglichen Teilbäume zu stark ein."
    | all (== 0) [and, or, equi] = reject $ do
        english "At least one of the following operators must have a frequency greater than 0: And, Or, Equi"
        german "Mindestens einer der folgenden Operatoren muss eine Frequenz größer als 0 besitzen: And, Or, Equi"
    | any (> 0) [impl, backImpl] = reject $ do
        english "Both implication operators are currently not implemented for this task."
        german "Beide Operatoren für Implikation sind derzeit für diese Aufgabe nicht implementiert."
    | otherwise = pure ()
data DecomposeFormulaInst = DecomposeFormulaInst
               { tree :: SynTree BinOp Char
               , addExtraHintsOnAssociativity :: Bool
               , addText :: Maybe (Map Language String)
               , showSolution :: Bool
               , unicodeAllowed :: Bool
               }
               deriving (Show, Typeable, Generic)

