{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SubTree.Quiz(
    generateSubTreeInst,
    ) where


import Data.Set (map, size)
import Trees.Generate (genSynTree)
import Test.QuickCheck (Gen, suchThat)

import Tasks.SubTree.Config (SubTreeConfig(..), SubTreeInst(..))
import Tasks.SynTree.Config (SynTreeConfig(..))
import Trees.Helpers (allNotLeafSubTrees, noSameSubTree)
import Trees.Print (display)




generateSubTreeInst :: SubTreeConfig -> Gen SubTreeInst
generateSubTreeInst SubTreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} = do
    tree <- genSynTree
        (minNodes, maxNodes)
        maxDepth
        usedLiterals
        atLeastOccurring
        allowArrowOperators
        maxConsecutiveNegations
        minUniqueBinOperators
      `suchThat` \synTree ->
        (allowSameSubTree || noSameSubTree synTree) && fromIntegral (size (allNotLeafSubTrees synTree)) >= minSubTrees
    let correctTrees = allNotLeafSubTrees tree
    return $ SubTreeInst
      { tree
      , minInputTrees = minSubTrees
      , correctFormulas = Data.Set.map display correctTrees
      , addText = extraText
      }
