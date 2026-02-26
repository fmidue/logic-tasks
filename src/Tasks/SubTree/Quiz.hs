{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SubTree.Quiz(
    generateSubTreeInst,
    ) where


import Data.Set (size)
import Trees.Generate (genSynTree)
import Test.QuickCheck (Gen, suchThat)

import Tasks.SubTree.Config (SubTreeConfig(..), SubTreeInst(..))
import Tasks.SynTree.Config (getArrows)
import Trees.Helpers (allNotLeafSubTrees, noSameSubTree)




generateSubTreeInst :: SubTreeConfig -> Gen SubTreeInst
generateSubTreeInst SubTreeConfig {..} = do
    tree <- genSynTree syntaxTreeConfig
      `suchThat` \synTree ->
        (allowSameSubTree || noSameSubTree synTree) && fromIntegral (size (allNotLeafSubTrees synTree)) >= subTreeAmount
    let correctTrees = allNotLeafSubTrees tree
    return $ SubTreeInst
      { tree
      , inputTreeAmount = subTreeAmount
      , correctTrees = correctTrees
      , arrowOperatorsToShow = getArrows syntaxTreeConfig
      , showSolution = printSolution
      , addText = extraText
      , unicodeAllowed = offerUnicodeInput
      }
