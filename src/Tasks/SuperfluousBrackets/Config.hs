{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Tasks.SuperfluousBrackets.Config (
    SuperfluousBracketsConfig (..),
    SuperfluousBracketsInst (..),
    defaultSuperfluousBracketsConfig,
    checkSuperfluousBracketsConfig
    )where


import Control.Monad.Output (LangM, OutputMonad, english, german)
import GHC.Generics (Generic)

import LogicTasks.Helpers (reject)
import Tasks.SynTree.Config(SynTreeConfig(..), checkSynTreeConfig, defaultSynTreeConfig)
import Trees.Types (BinOp, SynTree)




data SuperfluousBracketsConfig =
    SuperfluousBracketsConfig
    {
      syntaxTreeConfig :: SynTreeConfig
    , superfluousBracketPairs :: Integer
    } deriving (Show,Generic)



defaultSuperfluousBracketsConfig :: SuperfluousBracketsConfig
defaultSuperfluousBracketsConfig =
    SuperfluousBracketsConfig
    {
      syntaxTreeConfig = defaultSynTreeConfig { allowArrowOperators = True }
    , superfluousBracketPairs = 2
    }



checkSuperfluousBracketsConfig :: OutputMonad m => SuperfluousBracketsConfig -> LangM m
checkSuperfluousBracketsConfig config@SuperfluousBracketsConfig {..} =
    checkSynTreeConfig syntaxTreeConfig *> checkAdditionalConfig config



checkAdditionalConfig :: OutputMonad m => SuperfluousBracketsConfig -> LangM m
checkAdditionalConfig SuperfluousBracketsConfig {syntaxTreeConfig=SynTreeConfig {..}, ..}
    | minNodes < 5 = reject $ do
        english "Minimal number of nodes must larger than 4"
        german "Minimale Anzahl Blätter muss größer 4 sein."
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
    } deriving (Show,Generic)
