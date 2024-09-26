{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SuperfluousBrackets.Quiz (
    generateSuperfluousBracketsInst
    )where


import Test.QuickCheck (Gen, suchThat)

import Tasks.SuperfluousBrackets.Config (SuperfluousBracketsConfig(..), SuperfluousBracketsInst(..))
import Tasks.SuperfluousBrackets.PrintSuperfluousBrackets (superfluousBracketsDisplay)
import Tasks.SynTree.Config (SynTreeConfig(binOpFrequencies))
import Trees.Helpers (sameAssociativeOperatorAdjacent)
import Trees.Generate (genSynTree)
import Trees.Print (simplestDisplay)
import Trees.Types (BinOp(..))
import qualified Data.Map as Map (keys)




generateSuperfluousBracketsInst :: SuperfluousBracketsConfig -> Gen SuperfluousBracketsInst
generateSuperfluousBracketsInst SuperfluousBracketsConfig {..} = do
    tree <- genSynTree syntaxTreeConfig
      `suchThat` sameAssociativeOperatorAdjacent
    stringWithSuperfluousBrackets <- superfluousBracketsDisplay tree superfluousBracketPairs
    return $ SuperfluousBracketsInst
      { tree
      , stringWithSuperfluousBrackets
      , simplestString = simplestDisplay tree
      , showArrowOperators = any (`elem` Map.keys (binOpFrequencies syntaxTreeConfig)) [Impl, BackImpl, Equi]
      , showSolution = printSolution
      , addText = extraText
      , unicodeAllowed = offerUnicodeInput
      }
