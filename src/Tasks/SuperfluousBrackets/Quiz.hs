{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SuperfluousBrackets.Quiz (
    feedback,
    generateSuperfluousBracketsInst
)where

import Tasks.SuperfluousBrackets.Config (SuperfluousBracketsConfig(..), SuperfluousBracketsInst(..), )
import Tasks.SuperfluousBrackets.PrintSuperfluousBrackets(superfluousBracketsDisplay)
import Test.QuickCheck (Gen, suchThat)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Trees.Print (simplestDisplay)
import Trees.Helpers (sameAssociativeOperatorAdjacent)
import Trees.Generate (genSynTree)
import Trees.Types (PropFormula)




generateSuperfluousBracketsInst :: SuperfluousBracketsConfig -> Gen SuperfluousBracketsInst
generateSuperfluousBracketsInst SuperfluousBracketsConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} = do
    tree <- genSynTree
        (minNodes, maxNodes)
        maxDepth
        usedLiterals
        atLeastOccurring
        allowArrowOperators
        maxConsecutiveNegations
      `suchThat` sameAssociativeOperatorAdjacent
    stringWithSuperfluousBrackets <- superfluousBracketsDisplay tree superfluousBracketPairs
    return $ SuperfluousBracketsInst
      { tree
      , stringWithSuperfluousBrackets
      , simplestString = simplestDisplay tree
      }

feedback :: SuperfluousBracketsInst -> PropFormula Char -> Bool
feedback SuperfluousBracketsInst {simplestString} simForm = show simForm == simplestString
