{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module Tasks.SynTree.Quiz (
    generateSynTreeInst,
    ) where


import Test.QuickCheck (Gen)

import Tasks.SynTree.Config (SynTreeConfig(..), SynTreeInst(..))
import Trees.Generate (genSynTree)
import Trees.Print (display, transferToPicture)




generateSynTreeInst :: SynTreeConfig -> Gen SynTreeInst
generateSynTreeInst SynTreeConfig {..} = do
    tree <- genSynTree
      (minNodes, maxNodes)
      maxDepth
      usedLiterals
      atLeastOccurring
      allowArrowOperators
      maxConsecutiveNegations
      minUniqueBinOperators
    return $ SynTreeInst
      { tree
      , latexImage = transferToPicture tree
      , correct = display tree
      , addText = extraText
      , showSolution = printSolution
      , extraHintsOnSemanticEquivalence = extraHintsOnSemanticEquivalence
      }
