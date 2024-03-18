{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Tasks.SynTree.Config (
    SynTreeConfig(..),
    checkSynTreeConfig,
    defaultSynTreeConfig,
    ) where


import Control.Monad.Output (LangM, OutputMonad, english, german)
import Data.Char (isLetter)
import GHC.Generics (Generic)

import LogicTasks.Helpers (reject)
import Trees.Helpers (maxNodesForDepth)
import Trees.Types (BinOp)




data SynTreeConfig =
  SynTreeConfig
  { minNodes :: Integer
  , maxNodes :: Integer
  , minDepth :: Integer
  , maxDepth :: Integer
  , usedLiterals :: String
  , atLeastOccurring :: Integer
  , allowArrowOperators :: Bool
  , maxConsecutiveNegations :: Integer
  , minUniqueBinOperators :: Integer
  } deriving (Show,Generic)



defaultSynTreeConfig :: SynTreeConfig
defaultSynTreeConfig =
    SynTreeConfig
    { maxNodes = 10
    , minNodes = 6
    , minDepth = 3
    , maxDepth = 6
    , usedLiterals = "ABCDE"
    , atLeastOccurring = 3
    , allowArrowOperators = False
    , maxConsecutiveNegations = 2
    , minUniqueBinOperators = 0
    }



checkSynTreeConfig :: OutputMonad m => SynTreeConfig -> LangM m
checkSynTreeConfig SynTreeConfig {..}
    | not (all isLetter usedLiterals) = reject $ do
        english "Only letters are allowed as literals."
        german "Nur Buchstaben dürfen Literale sein."
    | fromIntegral (length usedLiterals) < atLeastOccurring = reject $ do
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
    | atLeastOccurring < 1 = reject $ do
        english "At least one literal occurs in each formula."
        german "Formel ohne Literale existiert nicht."
    | minNodes < atLeastOccurring * 2 - 1 = reject $ do
        english "Your minimum number of nodes does not permit enough leaves for all desired literals."
        german "Minimale Anzahl der Knoten ist zu niedrig um alle Literale zu verwenden."
    | minDepth < 1 = reject $ do
        english "Minimal depth must be positive"
        german "Minimale Tiefe muss positiv sein."
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

maxDepthForNodes :: Integer -> Integer -> Integer
maxDepthForNodes maxConsecutiveNegations nodes =
  let
    (result, rest) = (nodes - 1) `divMod` (maxConsecutiveNegations + 2)
  in
    1 + result * (maxConsecutiveNegations + 1) + min maxConsecutiveNegations rest
