{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tasks.SuperfluousBrackets.Config (
    SuperfluousBracketsConfig (..),
    SuperfluousBracketsInst (..),
    defaultSuperfluousBracketsConfig,
    checkSuperfluousBracketsConfig
    )where


import Control.OutputCapable.Blocks (LangM, Language, OutputCapable, english, german)
import GHC.Generics (Generic)
import Data.Map (Map)

import LogicTasks.Helpers (reject)
import Tasks.SynTree.Config(SynTreeConfig(..), checkSynTreeConfig, defaultSynTreeConfig)
import Trees.Types (BinOp(..), SynTree)
import qualified Data.Map as Map (fromList)



data SuperfluousBracketsConfig =
    SuperfluousBracketsConfig
    {
      syntaxTreeConfig :: SynTreeConfig
    , superfluousBracketPairs :: Integer
    , extraText :: Maybe (Map Language String)
    , printSolution :: Bool
    , offerUnicodeInput :: Bool
    } deriving (Show,Generic)



defaultSuperfluousBracketsConfig :: SuperfluousBracketsConfig
defaultSuperfluousBracketsConfig =
    SuperfluousBracketsConfig
    {
      syntaxTreeConfig = defaultSynTreeConfig
      { binOpFrequencies = Map.fromList
        [ (And, 1)
        , (Or, 1)
        , (Impl, 1)
        , (BackImpl, 1)
        , (Equi, 1)
        ]
      , minUniqueBinOperators = 2
      }
    , superfluousBracketPairs = 2
    , extraText = Nothing
    , printSolution = False
    , offerUnicodeInput = False
    }



checkSuperfluousBracketsConfig :: OutputCapable m => SuperfluousBracketsConfig -> LangM m
checkSuperfluousBracketsConfig config@SuperfluousBracketsConfig {..} =
    checkSynTreeConfig syntaxTreeConfig *> checkAdditionalConfig config



checkAdditionalConfig :: OutputCapable m => SuperfluousBracketsConfig -> LangM m
checkAdditionalConfig SuperfluousBracketsConfig {syntaxTreeConfig=SynTreeConfig {..}, superfluousBracketPairs}
    | minUniqueBinOperators < 1 = reject $ do
        english "There should be a positive number of (unique) operators."
        german "Es sollte eine positive Anzahl an (unterschiedlichen) Operatoren geben."
    | minNodes < 2 * minUniqueBinOperators + 3 = reject $ do
        english "Minimal number of nodes must larger, given the desired number of unique operators."
        german "Minimale Anzahl Knoten muss größer sein, angesichts der angestrebten Anzahl unterschiedlicher Operatoren."
    | superfluousBracketPairs > minNodes `div` 2 = reject $ do
        english "The number of superfluous brackets is excessive, given your node numbers."
        german "Die Anzahl zusätzlicher Klammern ist zu hoch für die Menge an Blättern."
    | superfluousBracketPairs < 1 = reject $ do
        english "Add at least one extra pair of brackets."
        german "Es muss mindestens ein Klammerpaar hinzugefügt werden."
    | otherwise = pure()



data SuperfluousBracketsInst =
    SuperfluousBracketsInst
    {
      tree :: SynTree BinOp Char
    , stringWithSuperfluousBrackets :: String
    , simplestString :: String
    , showArrowOperators :: Bool
    , showSolution :: Bool
    , addText :: Maybe (Map Language String)
    , unicodeAllowed :: Bool
    } deriving (Show,Generic)
