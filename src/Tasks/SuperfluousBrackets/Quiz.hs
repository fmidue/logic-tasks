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
generateSuperfluousBracketsInst SuperfluousBracketsConfig {..} = do
    tree <- genSynTree syntaxTreeConfig
      `suchThat` sameAssociativeOperatorAdjacent
    stringWithSuperfluousBrackets <- superfluousBracketsDisplay tree superfluousBracketPairs
    return $ SuperfluousBracketsInst
      { tree
      , stringWithSuperfluousBrackets
      , simplestString = simplestDisplay tree
      , showArrowOperators = allowArrowOperators syntaxTreeConfig
      , showSolution = printSolution
      , addText = extraText
      }
