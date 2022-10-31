{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Tasks.SynTree.Config (
  SynTreeConfig(..),
  SynTreeInst(..),
  checkSynTreeConfig,
  defaultSynTreeConfig,
  ) where


import Control.Monad.Output (LangM, OutputMonad(..), english, german, translate)
import Data.Char (isLetter)
import GHC.Generics

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
    | not (all isLetter usedLiterals)
      = reject "Only letters are allowed as literals."
               "Nur Buchstaben dürfen Literale sein."
    | maxConsecutiveNegations < 0
      = reject "Minimal number of consecutive negations must not be negative"
               "Minimale Anzahl aufeinander folgender Negationen kann nicht negativ sein."
    | maxConsecutiveNegations == 0 && (even maxNodes || even minNodes)
      = reject "Syntax tree with no negation can not have even nodes"
               "Syntaxbaum ohne Negation kann keine gerade Anzahl Blätter haben."
    | minNodes < 1
      = reject "Minimal number of nodes must be positive."
               "Minimale Anzahl Blätter muss positiv sein."
    | maxNodes < minNodes
      = reject "Maximal number of nodes must not be smaller than minimal number."
               "Maximale Anzahl Blätter ist kleiner als minimale."
    | maxDepth < 1
      = reject "Non-positive depth makes no sense."
               "Baum hat negative Tiefe."
    | atLeastOccurring < 1
      = reject "At least one literal occurs in each formula."
               "Formel ohne Literale existiert nicht."
    | fromIntegral (length usedLiterals) < atLeastOccurring
      = reject "You have provided too few literals."
               "Anzahl Literale ist zu niedrig für gegebene Einstellungen."
    | minNodes < atLeastOccurring * 2 - 1
      = reject "Your minimum number of nodes does not permit enough leaves for all desired literals."
               "Minimale Anzahl der Blätter ist zu niedrig um alle Literale zu verwenden."
    | maxNodes > maxNodesForDepth maxDepth
      = reject "Your minimum number of nodes is larger than what your maximum depth enables."
               "Minimale Anzahl der Blätter würde eingestellte Tiefe verletzen."
    | let maxNodes' = maxNodes - 1
          maxConsecutiveNegations' = maxConsecutiveNegations + 2
          (result, rest) = maxNodes' `divMod` maxConsecutiveNegations', maxDepth > 1 + result * (maxConsecutiveNegations + 1) + min maxConsecutiveNegations rest
      = reject "Your maximum depth value is unreasonably large, given your other settings."
               "Maximale Tiefe des Baumes ist zu hoch für eingestellte Parameter."
    | otherwise = pure()
  where
    reject e g  = refuse $ indent $ translate $ do
      english e
      german g



data SynTreeInst =
    SynTreeInst
    { instSynTree :: SynTree BinOp Char
    , latexImage :: String
    , correct :: String
    } deriving (Show,Generic)
