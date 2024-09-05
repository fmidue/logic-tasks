{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tasks.LegalProposition.Config (
    LegalPropositionConfig (..),
    LegalPropositionInst (..),
    checkLegalPropositionConfig,
    defaultLegalPropositionConfig,
    ) where


import Control.OutputCapable.Blocks (LangM, Language, OutputCapable, english, german)
import GHC.Generics (Generic)
import Data.Map (Map)

import LogicTasks.Helpers (reject)
import Trees.Helpers (maxLeavesForNodes)
import Tasks.SynTree.Config(SynTreeConfig(..), checkSynTreeConfig, defaultSynTreeConfig)
import Trees.Types (SynTree, BinOp)
import Tasks.LegalProposition.Helpers (formulaAmount)




data LegalPropositionConfig =
    LegalPropositionConfig
    {
      syntaxTreeConfig :: SynTreeConfig
    , formulas :: Integer
    , illegals :: Integer
    , bracketFormulas :: Integer
    , extraText :: Maybe (Map Language String)
    , printSolution :: Bool
    } deriving (Show,Generic)

defaultLegalPropositionConfig :: LegalPropositionConfig
defaultLegalPropositionConfig =
    LegalPropositionConfig
    {
      syntaxTreeConfig = defaultSynTreeConfig
    , formulas = 5
    , illegals = 2
    , bracketFormulas = 1
    , extraText = Nothing
    , printSolution = False
    }

checkLegalPropositionConfig :: OutputCapable m => LegalPropositionConfig -> LangM m
checkLegalPropositionConfig config@LegalPropositionConfig {..} =
    checkSynTreeConfig syntaxTreeConfig *> checkAdditionalConfig config


checkAdditionalConfig :: OutputCapable m => LegalPropositionConfig -> LangM m
checkAdditionalConfig config@LegalPropositionConfig {syntaxTreeConfig = SynTreeConfig {..}, formulas, illegals, bracketFormulas}
    | minNodes < 3 = reject $ do
        english "form A and ~A is meaningless in this kind of issue"
        german "Minimale Anzahl an Blättern unter 3 kann nur triviale Aufgaben erzeugen."
    | formulas < 1 = reject $ do
        english "The number of formulas must be positive."
        german "Anzahl der Formeln muss positiv sein."
    | illegals < 0 = reject $ do
        english "The number of illegals cannot be negative."
        german "Anzahl falscher Formeln muss 0 oder höher sein."
    | bracketFormulas < 0 = reject $ do
        english "The number of bracketFormulas cannot be less than 0."
        german "bracketFormulas kann nicht negativ sein."
    | formulas < illegals + bracketFormulas = reject $ do
        english "The number of formulas cannot be less than the sum of bracket Formulas and illegal ones."
        german "Die Anzahl der Formeln kann nicht niedriger als die Summe von falschen und richtigen Formeln."
    | let leaves = maxLeavesForNodes maxNodes, (if allowArrowOperators then 4 else 2) ^ (maxNodes - leaves) < formulas
      = reject $ do
        english "Settings may result in extremely large formulas."
        german "Einstellungen führen zu extrem großen Formeln."
    | formulaAmount (syntaxTreeConfig config) < formulas = reject $ do
      english "Settings cannot ensure provided amount of formulas."
      german "Einstellungen können nicht die Anzahl der geforderten Formeln erfüllen."
    | otherwise = pure()



data LegalPropositionInst =
    LegalPropositionInst
    {
      pseudoFormulas :: [(String, Maybe (SynTree BinOp Char))]
    , showSolution :: Bool
    , addText :: Maybe (Map Language String)
    } deriving (Show,Generic)
