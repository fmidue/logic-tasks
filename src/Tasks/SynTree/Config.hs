{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Tasks.SynTree.Config (
    SynTreeConfig(..),
    checkSynTreeConfig,
    defaultSynTreeConfig,
    ) where


import Control.OutputCapable.Blocks (LangM, OutputCapable, english, german)
import Data.Char (isLetter)
import GHC.Generics (Generic)

import LogicTasks.Helpers (reject)
import Trees.Helpers (maxNodesForDepth, maxDepthForNodes)
import Trees.Types (BinOp)

import Data.List.Extra (nubOrd)


data SynTreeConfig =
  SynTreeConfig
  { minNodes :: Integer
  , maxNodes :: Integer
  , minDepth :: Integer
  , maxDepth :: Integer
  , availableAtoms :: String
  , minAmountOfUniqueAtoms :: Integer
  , allowArrowOperators :: Bool
  , maxConsecutiveNegations :: Integer
  , minUniqueBinOperators :: Integer
  } deriving (Show,Generic)



defaultSynTreeConfig :: SynTreeConfig
defaultSynTreeConfig =
    SynTreeConfig
    { maxNodes = 10
    , minNodes = 7
    , minDepth = 3
    , maxDepth = 6
    , availableAtoms = "ABCDE"
    , minAmountOfUniqueAtoms = 3
    , allowArrowOperators = False
    , maxConsecutiveNegations = 2
    , minUniqueBinOperators = 1
    }



checkSynTreeConfig :: OutputCapable m => SynTreeConfig -> LangM m
checkSynTreeConfig SynTreeConfig {..}
    | not (all isLetter availableAtoms) = reject $ do
        english "Only letters are allowed as literals."
        german "Nur Buchstaben dürfen Literale sein."
    | length availableAtoms /= length (nubOrd availableAtoms) = reject $ do
        english "No letter should be given as possible literal twice."
        german "Kein Buchstabe darf mehrmals als mögliches Literal genannt sein."
    | minUniqueBinOperators == 0 && minAmountOfUniqueAtoms > 1 = reject $ do
        english "Without binary operators there cannot be more than one leaf node."
        german "Ohne binäre Operatoren kann es nicht mehr als einen Blattknoten geben."
    | fromIntegral (length availableAtoms) < minAmountOfUniqueAtoms = reject $ do
        english "You have provided too few literals."
        german "Anzahl Literale ist zu niedrig für gegebene Einstellungen."
    | maxConsecutiveNegations < 0 = reject $ do
        english "Maximal number of consecutive negations must not be negative"
        german "Maximale Anzahl aufeinander folgender Negationen kann nicht negativ sein."
    | maxConsecutiveNegations == 0 && (even maxNodes || even minNodes) = reject $ do
        english "Syntax tree with no negation cannot have even number of nodes."
        german "Syntaxbaum ohne Negation kann keine gerade Anzahl Knoten haben."
    | maxConsecutiveNegations >= maxDepth = reject $ do
        english "The maximum number of consecutive negations cannot reach or exceed the maximum depth."
        german "Die maximale Anzahl aufeinanderfolgender Negationen kann die maximale Tiefe nicht erreichen oder überschreiten."
    | minAmountOfUniqueAtoms < 1 = reject $ do
        english "At least one literal occurs in each formula."
        german "Formel ohne Literale existiert nicht."
    | minNodes < minAmountOfUniqueAtoms * 2 - 1 = reject $ do
        english "Your minimum number of nodes does not permit enough leaves for all desired literals."
        german "Minimale Anzahl der Knoten ist zu niedrig um alle Literale zu verwenden."
    | minNodes <= 2 * minUniqueBinOperators = reject $ do
        english "The minimal number of nodes is incompatible with the minimal number of unique operators"
        german "Die minimale Anzahl der Knoten erlaubt nicht die minimale Anzahl an unterschiedlichen Operatoren"
    | minNodes > maxNodesForDepth minDepth = reject $ do
        english "Minimum number of nodes does not allow a tree with minimum depth."
        german "Minimale Anzahl an Knoten ermöglicht keinen Baum mit minimaler Tiefe."
    | minDepth > maxDepthForNodes maxConsecutiveNegations minNodes
      = reject $ do
        english "Your minimum depth value is unreasonably large, given your other settings."
        german "Minimale Tiefe des Baumes ist zu hoch für eingestellte Parameter."
    | maxNodes < minNodes = reject $ do
        english "Maximal number of nodes must not be smaller than minimal number."
        german "Maximale Anzahl Knoten ist kleiner als minimale."
    | maxDepth < minDepth = reject $ do
        english "Maximal depth must not be smaller than minimal depth."
        german "Maximale Tiefe ist kleiner als minimale Tiefe."
    | maxNodes > maxNodesForDepth maxDepth = reject $ do
        english "Your maximum number of nodes is larger than what your maximum depth enables."
        german "Maximale Anzahl der Knoten würde eingestellte maximale Tiefe verletzen."
    | maxDepth > maxDepthForNodes maxConsecutiveNegations maxNodes
      = reject $ do
        english "Your maximum depth value is unreasonably large, given your other settings."
        german "Maximale Tiefe des Baumes ist zu hoch für eingestellte Parameter."
    | minUniqueBinOperators < 0 = reject $ do
        english "There should be a non-negative number of unique operators."
        german "Es sollte eine nicht-negative Anzahl an unterschiedlichen Operatoren geben."
    | minUniqueBinOperators > fromIntegral (length [minBound .. maxBound :: BinOp]) = reject $ do
        english "The number of unique operators cannot exceed the maximum number of operators."
        german "Die Anzahl der unterschiedlichen Operatoren kann nicht die maximale Anzahl überschreiten."
    | not allowArrowOperators && minUniqueBinOperators > 2 = reject $ do
        english "This number of unique operators cannot be reached with allowArrowOperators = False ."
        german "Die angegebene Anzahl der unterschiedlichen Operatoren kann mit allowArrowOperators = False nicht erreicht werden."
    | otherwise = pure()
