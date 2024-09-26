{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SubTree.Quiz(
    generateSubTreeInst,
    ) where


import Data.Set (size)
import Trees.Generate (genSynTree)
import Test.QuickCheck (Gen, suchThat)

import Tasks.SubTree.Config (SubTreeConfig(..), SubTreeInst(..))
import Tasks.SynTree.Config (SynTreeConfig(binOpFrequencies))
import Trees.Helpers (allNotLeafSubTrees, noSameSubTree)
import Trees.Types (BinOp(..))
import qualified Data.Map as Map (keys)




generateSubTreeInst :: SubTreeConfig -> Gen SubTreeInst
generateSubTreeInst SubTreeConfig {..} = do
    tree <- genSynTree syntaxTreeConfig
      `suchThat` \synTree ->
        (allowSameSubTree || noSameSubTree synTree) && fromIntegral (size (allNotLeafSubTrees synTree)) >= minSubTrees
    let correctTrees = allNotLeafSubTrees tree
    return $ SubTreeInst
      { tree
      , minInputTrees = minSubTrees
      , correctTrees = correctTrees
      , showArrowOperators = any (`elem` Map.keys (binOpFrequencies syntaxTreeConfig)) [Impl, BackImpl, Equi]
      , showSolution = printSolution
      , addText = extraText
      , unicodeAllowed = offerUnicodeInput
      }
