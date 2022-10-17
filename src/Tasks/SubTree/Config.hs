{-# LANGUAGE RecordWildCards #-}

module Tasks.SubTree.Config (
    SubTreeInst(..),
    SubTreeConfig(..),
    defaultSubTreeConfig,
    checkSubTreeConfig
  ) where



import Data.Set (Set)
import Control.Monad.Output(LangM, OutputMonad(..), english, german, translate)
import Tasks.SynTree.Config(SynTreeConfig(..), checkSynTreeConfig, defaultSynTreeConfig)
import Trees.Types (SynTree, BinOp)
import Trees.Helpers (maxLeavesForNodes)



data SubTreeConfig =
  SubTreeConfig
    {
      syntaxTreeConfig :: SynTreeConfig
    , allowSameSubTree :: Bool
    , minSubTrees :: Integer
    } deriving Show

defaultSubTreeConfig :: SubTreeConfig
defaultSubTreeConfig =
    SubTreeConfig
    { syntaxTreeConfig = defaultSynTreeConfig
    , allowSameSubTree = True
    , minSubTrees = 3
    }

checkSubTreeConfig :: OutputMonad m => SubTreeConfig -> LangM m
checkSubTreeConfig subConfig@SubTreeConfig {..} =
    checkSynTreeConfig syntaxTreeConfig >> checkAdditionalConfig subConfig


checkAdditionalConfig :: OutputMonad m => SubTreeConfig -> LangM m
checkAdditionalConfig SubTreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..}
    | minSubTrees < 2
      = reject "The task makes no sense if not at least two subtrees are generated."
               "Es müssen mindestens zwei Unterbäume erzeugt werden."
    | minNodes - maxLeavesForNodes minNodes < minSubTrees
      = reject "In this case, it is possible to have too many leaves nodes. This leads to not having enough non-atomic SubTrees."
               "MIt diesen Einstellungen können nicht genügend nicht-triviale Unterbäume erzeugt werden."
    | otherwise = pure()
  where
    reject e g  = refuse $ indent $ translate $ do
      english e
      german g

data SubTreeInst =
    SubTreeInst
    { formula :: String
    , correctTrees :: Set (SynTree BinOp Char)
    , correctFormulas :: Set String
    , minInputTrees :: Integer
    } deriving Show
