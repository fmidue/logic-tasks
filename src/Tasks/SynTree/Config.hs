{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Tasks.SynTree.Config (
    SynTreeConfig(..),
    SynTreeInst(..),
    checkSynTreeConfig,
    defaultSynTreeConfig,
    ) where


import Control.Monad.Output (LangM, OutputMonad, english, german)
import Data.Char (isLetter)
import GHC.Generics (Generic)

import LogicTasks.Helpers (reject)
import Trees.Helpers (maxNodesForDepth)
import Trees.Types (SynTree, BinOp)




data SynTreeConfig =
  SynTreeConfig
  { minNodes :: Integer
  , maxNodes :: Integer
  , maxDepth :: Integer
  , usedLiterals :: String
  , atLeastOccurring :: Integer
  , allowArrowOperators :: Bool
  , maxConsecutiveNegations :: Integer
  } deriving (Show,Generic)



defaultSynTreeConfig :: SynTreeConfig
defaultSynTreeConfig =
    SynTreeConfig
    { maxNodes = 10
    , minNodes = 6
    , maxDepth = 6
    , usedLiterals = "ABCDE"
    , atLeastOccurring = 3
    , allowArrowOperators = False
    , maxConsecutiveNegations = 2
    }



checkSynTreeConfig :: OutputMonad m => SynTreeConfig -> LangM m
checkSynTreeConfig SynTreeConfig {..}
    | not (all isLetter usedLiterals) = reject $ do
        english "Only letters are allowed as literals."
        german "Nur Buchstaben dürfen Literale sein."
    | maxConsecutiveNegations < 0 = reject $ do
        english "Minimal number of consecutive negations must not be negative"
        german "Minimale Anzahl aufeinander folgender Negationen kann nicht negativ sein."
    | maxConsecutiveNegations == 0 && (even maxNodes || even minNodes) = reject $ do
        english "Syntax tree with no negation can not have even nodes"
        german "Syntaxbaum ohne Negation kann keine gerade Anzahl Blätter haben."
    | minNodes < 1 = reject$ do
        english"Minimal number of nodes must be positive."
        german "Minimale Anzahl Blätter muss positiv sein."
    | maxNodes < minNodes = reject $ do
        english "Maximal number of nodes must not be smaller than minimal number."
        german "Maximale Anzahl Blätter ist kleiner als minimale."
    | maxDepth < 1 = reject $ do
        english "Non-positive depth makes no sense."
        german "Baum hat negative Tiefe."
    | atLeastOccurring < 1 = reject $ do
        english "At least one literal occurs in each formula."
        german "Formel ohne Literale existiert nicht."
    | fromIntegral (length usedLiterals) < atLeastOccurring = reject $ do
        english "You have provided too few literals."
        german "Anzahl Literale ist zu niedrig für gegebene Einstellungen."
    | minNodes < atLeastOccurring * 2 - 1 = reject $ do
        english "Your minimum number of nodes does not permit enough leaves for all desired literals."
        german "Minimale Anzahl der Blätter ist zu niedrig um alle Literale zu verwenden."
    | maxNodes > maxNodesForDepth maxDepth = reject $ do
        english "Your minimum number of nodes is larger than what your maximum depth enables."
        german "Minimale Anzahl der Blätter würde eingestellte Tiefe verletzen."
    | let maxNodes' = maxNodes - 1
          maxConsecutiveNegations' = maxConsecutiveNegations + 2
          (result, rest) =
            maxNodes' `divMod` maxConsecutiveNegations',
            maxDepth > 1 + result * (maxConsecutiveNegations + 1) + min maxConsecutiveNegations rest
      = reject $ do
        english "Your maximum depth value is unreasonably large, given your other settings."
        german "Maximale Tiefe des Baumes ist zu hoch für eingestellte Parameter."
    | otherwise = pure()



data SynTreeInst =
    SynTreeInst
    { tree :: SynTree BinOp Char
    , latexImage :: String
    , correct :: String
    } deriving (Show,Generic)
