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
import Trees.Types (PropFormula(..))




generateSuperfluousBracketsInst :: SuperfluousBracketsConfig -> Gen SuperfluousBracketsInst
generateSuperfluousBracketsInst SuperfluousBracketsConfig {syntaxTreeConfig = SynTreeConfig {..}, ..} = do
    syntaxTree <- genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring allowArrowOperators maxConsecutiveNegations
      `suchThat` sameAssociativeOperatorAdjacent
    stringWithSuperfluousBrackets <- superfluousBracketsDisplay syntaxTree superfluousBracketPairs
    return $ SuperfluousBracketsInst
      { syntaxTree
      , stringWithSuperfluousBrackets
      , simplestString = simplestDisplay syntaxTree
      }

feedback :: SuperfluousBracketsInst -> PropFormula -> Bool
feedback SuperfluousBracketsInst {simplestString} simForm = show simForm == simplestString
