{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Tasks.SuperfluousBrackets.Config(
    SuperfluousBracketsConfig (..),
    SuperfluousBracketsInst (..),
    defaultSuperfluousBracketsConfig,
    checkSuperfluousBracketsConfig
)where


import Control.Monad.Output(LangM, OutputMonad(..), english, german, translate)
import GHC.Generics

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
checkSuperfluousBracketsConfig sBConfig@SuperfluousBracketsConfig {..} =
    checkSynTreeConfig syntaxTreeConfig >> checkAdditionalConfig sBConfig

checkAdditionalConfig :: OutputMonad m => SuperfluousBracketsConfig -> LangM m
checkAdditionalConfig SuperfluousBracketsConfig {syntaxTreeConfig=SynTreeConfig {..}, ..}
    | minNodes < 5
      = reject "Minimal number of nodes must larger than 4"
               "Minimale Anzahl Blätter muss größer 4 sein."
    | superfluousBracketPairs > minNodes `div` 2
      = reject "The number of superfluous brackets is excessive, given your node numbers."
               "Die Anzahl zusätzlicher Klammern ist zu hoch für die Menge an Blättern."
    | superfluousBracketPairs < 1
      = reject "Add at least one extra pair of brackets."
               "Es muss mindestens ein KLammerpaar hinzugefügt werden."
    | otherwise
      = pure()
  where
    reject e g  = refuse $ indent $ translate $ do
      english e
      german g



data SuperfluousBracketsInst =
    SuperfluousBracketsInst
    {
      syntaxTree :: SynTree BinOp Char
    , stringWithSuperfluousBrackets :: String
    , simplestString :: String
    } deriving (Show,Generic)
