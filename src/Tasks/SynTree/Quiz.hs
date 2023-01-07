{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SynTree.Quiz (
    feedback,
    generateSynTreeInst,
    ) where

import Test.QuickCheck (Gen)
import Tasks.SynTree.Config (SynTreeConfig(..), SynTreeInst(..))

import Trees.Helpers (formulaToTree)
import Trees.Print (display, transferToPicture)
import Trees.Generate (genSynTree)
import Trees.Types (PropFormula(..))



generateSynTreeInst :: SynTreeConfig -> Gen SynTreeInst
generateSynTreeInst SynTreeConfig {..} = do
    tree <- genSynTree (minNodes, maxNodes) maxDepth usedLiterals atLeastOccurring allowArrowOperators maxConsecutiveNegations
    return $ SynTreeInst
      { instSynTree = tree
      , latexImage = transferToPicture tree
      , correct = display tree
      }

feedback :: SynTreeInst -> PropFormula -> Bool
feedback SynTreeInst {instSynTree} input =
  formulaToTree input == instSynTree
