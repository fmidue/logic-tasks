{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Tasks.SubTree.Config (
    SubTreeInst(..),
    SubTreeConfig(..),
    defaultSubTreeConfig,
    checkSubTreeConfig
    ) where


import Control.Monad.Output (LangM, OutputMonad, english, german, Language)
import Data.Set (Set)
import GHC.Generics (Generic)
import Data.Map (Map)

import LogicTasks.Helpers (reject)
import Tasks.SynTree.Config(SynTreeConfig(..), checkSynTreeConfig, defaultSynTreeConfig)
import Trees.Helpers (maxLeavesForNodes)
import Trees.Types (BinOp, SynTree)




data SubTreeConfig =
  SubTreeConfig
    {
      syntaxTreeConfig :: SynTreeConfig
    , allowSameSubTree :: Bool
    , minSubTrees :: Integer
    , printSolution :: Bool
    } deriving (Show,Generic)



defaultSubTreeConfig :: SubTreeConfig
defaultSubTreeConfig =
    SubTreeConfig
    { syntaxTreeConfig = defaultSynTreeConfig
    , allowSameSubTree = True
    , minSubTrees = 3
    , printSolution = False
    }



checkSubTreeConfig :: OutputMonad m => SubTreeConfig -> LangM m
checkSubTreeConfig subConfig@SubTreeConfig {..} =
    checkSynTreeConfig syntaxTreeConfig *> checkAdditionalConfig subConfig



checkAdditionalConfig :: OutputMonad m => SubTreeConfig -> LangM m
checkAdditionalConfig SubTreeConfig {syntaxTreeConfig = SynTreeConfig {..}, minSubTrees}
    | minSubTrees < 2 = reject $ do
        english "The task makes no sense if not at least two subtrees are generated."
        german "Es müssen mindestens zwei Unterbäume erzeugt werden."
    | minNodes - maxLeavesForNodes minNodes < minSubTrees = reject $ do
        english "These settings do not allow for enough non-atomic subtrees."
        german "Mit diesen Einstellungen können nicht genügend nicht-triviale Unterbäume erzeugt werden."
    | otherwise = pure()



data SubTreeInst =
    SubTreeInst
    { tree :: SynTree BinOp Char
    , correctTrees :: Set (SynTree BinOp Char)
    , minInputTrees :: Integer
    , showSolution :: Bool
    , addText :: Maybe (Map Language String)
    } deriving (Show,Generic)
