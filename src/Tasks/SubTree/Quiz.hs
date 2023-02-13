{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SubTree.Quiz(
    feedback,
    generateSubTreeInst,
) where

import Test.QuickCheck (Gen, suchThat)
import Trees.Generate (genSynTree)
import Data.Set (fromList, isSubsetOf, size, map)

import Tasks.SubTree.Config (SubTreeConfig(..), SubTreeInst(..), SubTreeInst)
import Trees.Print (display)
import Trees.Types (PropFormula)
import Trees.Helpers (allNotLeafSubTrees, noSameSubTree)
import Tasks.SynTree.Config (SynTreeConfig(..))


generateSubTreeInst :: SubTreeConfig -> Gen SubTreeInst
generateSubTreeInst SubTreeConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} = do
    tree <- genSynTree
        (minNodes, maxNodes)
        maxDepth
        usedLiterals
        atLeastOccurring
        allowArrowOperators
        maxConsecutiveNegations
      `suchThat` \synTree ->
        (allowSameSubTree || noSameSubTree synTree) && fromIntegral (size (allNotLeafSubTrees synTree)) >= minSubTrees
    let correctTrees = allNotLeafSubTrees tree
    return $ SubTreeInst
      { tree
      , minInputTrees = minSubTrees
      , correctFormulas = Data.Set.map display correctTrees
      }

feedback :: SubTreeInst -> [PropFormula Char] -> Bool
feedback SubTreeInst {correctFormulas} ps = inputSet `isSubsetOf` correctFormulas
  where
    inputSet = fromList (Prelude.map show ps)
