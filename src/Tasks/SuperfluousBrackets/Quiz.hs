{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SuperfluousBrackets.Quiz (
    generateSuperfluousBracketsInst
    )where


import Test.QuickCheck (Gen, suchThat)

import Tasks.SuperfluousBrackets.Config (SuperfluousBracketsConfig(..), SuperfluousBracketsInst(..))
import Tasks.SuperfluousBrackets.PrintSuperfluousBrackets (superfluousBracketsDisplay)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Trees.Helpers (sameAssociativeOperatorAdjacent, numOfUniqueBinOpsInSynTree)
import Trees.Generate (genSynTree)
import Trees.Print (simplestDisplay)




generateSuperfluousBracketsInst :: SuperfluousBracketsConfig -> Gen SuperfluousBracketsInst
generateSuperfluousBracketsInst SuperfluousBracketsConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} = do
    tree <- genSynTree
        (minNodes, maxNodes)
        maxDepth
        usedLiterals
        atLeastOccurring
        allowArrowOperators
        maxConsecutiveNegations
      `suchThat` \x -> sameAssociativeOperatorAdjacent x && numOfUniqueBinOpsInSynTree x >= minUniqueBinOperators
    stringWithSuperfluousBrackets <- superfluousBracketsDisplay tree superfluousBracketPairs
    return $ SuperfluousBracketsInst
      { tree
      , stringWithSuperfluousBrackets
      , simplestString = simplestDisplay tree
      , extraText = extraText
      }
