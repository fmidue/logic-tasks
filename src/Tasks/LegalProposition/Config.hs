{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Tasks.LegalProposition.Config (
    LegalPropositionConfig (..),
    LegalPropositionInst (..),

    defaultLegalPropositionConfig,
    checkLegalPropositionConfig
) where


import Control.Monad.Output (LangM, OutputMonad(..), english, german, translate)
import Data.Set (Set)
import GHC.Generics

import Trees.Helpers (maxLeavesForNodes)
import Tasks.SynTree.Config(SynTreeConfig(..), checkSynTreeConfig, defaultSynTreeConfig)




data LegalPropositionConfig =
    LegalPropositionConfig
    {
      syntaxTreeConfig :: SynTreeConfig
    , formulas :: Integer
    , illegals :: Integer
    , bracketFormulas :: Integer
    } deriving (Show,Generic)

defaultLegalPropositionConfig :: LegalPropositionConfig
defaultLegalPropositionConfig =
    LegalPropositionConfig
    {
      syntaxTreeConfig = defaultSynTreeConfig
    , formulas = 5
    , illegals = 2
    , bracketFormulas = 1
    }

checkLegalPropositionConfig :: OutputMonad m => LegalPropositionConfig ->LangM m
checkLegalPropositionConfig lPConfig@LegalPropositionConfig {..} =
    checkSynTreeConfig syntaxTreeConfig >> checkAdditionalConfig lPConfig


checkAdditionalConfig :: OutputMonad m => LegalPropositionConfig ->LangM m
checkAdditionalConfig LegalPropositionConfig {syntaxTreeConfig = SynTreeConfig {..}, ..}
    | minNodes < 3
      = reject "form A and ~A is meaningless in this kind of issue"
               "Minimale Anzahl an Blättern unter 3 kann nur triviale Aufgaben erzeugen."
    | formulas < 1
      = reject "The number of formulas must be positive."
               "Anzahl der Formeln muss positiv sein."
    | illegals < 0
      = reject "The number of illegals can not be negative."
               "Anzahl falscher Formeln muss 0 oder höher sein."
    | bracketFormulas < 0
      = reject "The number of bracketFormulas cannot be less than 0."
               "bracketFormulas kann nicht negativ sein."
    | formulas < illegals + bracketFormulas
      = reject "The number of formulas cannot be less than the sum of bracket Formulas and illegal ones."
               "Die Anzahl der Formeln kann nicht niedriger als die Summe von falschen und richtigen Formeln."
    | let leaves = maxLeavesForNodes maxNodes, (if allowArrowOperators then 4 else 2) ^ (maxNodes - leaves) < formulas
      = reject "Settings may result in extremely large formulae."
               "Einstellungen führen zu extrem großen Formeln."
    | otherwise
      = pure()
  where
    reject e g  = refuse $ indent $ translate $ do
      english e
      german g

data LegalPropositionInst =
    LegalPropositionInst
    {
      serialsOfWrong :: Set Int
    , pseudoFormulas :: [String]
    } deriving (Show,Generic)
