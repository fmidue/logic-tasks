{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SuperfluousBrackets.Quiz (
    generateSuperfluousBracketsInst
    )where


import Test.QuickCheck (Gen, suchThat)

import Tasks.SuperfluousBrackets.Config (SuperfluousBracketsConfig(..), SuperfluousBracketsInst(..))
import Tasks.SuperfluousBrackets.PrintSuperfluousBrackets (superfluousBracketsDisplay)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Trees.Helpers (sameAssociativeOperatorAdjacent)
import Trees.Generate (genSynTree)
import Trees.Print (simplestDisplay)




generateSuperfluousBracketsInst :: SuperfluousBracketsConfig -> Gen SuperfluousBracketsInst
generateSuperfluousBracketsInst SuperfluousBracketsConfig {syntaxTreeConfig = SynTreeConfig {minNodes, maxNodes, maxDepth, usedLiterals, atLeastOccurring, allowArrowOperators, maxConsecutiveNegations, minUniqueBinOperators, extraText}, ..} = do
    tree <- genSynTree
        (minNodes, maxNodes)
        maxDepth
        usedLiterals
        atLeastOccurring
        allowArrowOperators
        maxConsecutiveNegations
        minUniqueBinOperators
      `suchThat` sameAssociativeOperatorAdjacent
    stringWithSuperfluousBrackets <- superfluousBracketsDisplay tree superfluousBracketPairs
    return $ SuperfluousBracketsInst
      { tree
      , stringWithSuperfluousBrackets
      , simplestString = simplestDisplay tree
      , showSolution = printSolution
      , addText = extraText
      }
