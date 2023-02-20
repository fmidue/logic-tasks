{-# LANGUAGE RecordWildCards #-}

module Tasks.LegalProposition.Quiz (
    generateLegalPropositionInst,
    ) where


import Data.Char (isLetter)
import Data.Set (fromList)
import Test.QuickCheck (Gen, choose, suchThat, vectorOf)

import Auxiliary (listNoDuplicate)
import Tasks.LegalProposition.Config (LegalPropositionConfig (..), LegalPropositionInst (..))
import Tasks.LegalProposition.PrintBracket (bracketDisplay)
import Tasks.LegalProposition.PrintIllegal (illegalDisplay)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Trees.Generate (genSynTree)
import Trees.Helpers (similarExist)
import Trees.Print (display)
import Trees.Types (BinOp, SynTree)




generateLegalPropositionInst :: LegalPropositionConfig -> Gen LegalPropositionInst
generateLegalPropositionInst LegalPropositionConfig  {syntaxTreeConfig = SynTreeConfig {..}, ..} = do
    treeList <- vectorOf
        (fromIntegral formulas)
        (genSynTree (minNodes, maxNodes)
          maxDepth
          usedLiterals
          atLeastOccurring
          allowArrowOperators
          maxConsecutiveNegations
        )
      `suchThat` (not . similarExist)
    serialsOfWrong <- vectorOf (fromIntegral illegals) (choose (1, fromIntegral formulas) )`suchThat` listNoDuplicate
    serialsOfBracket <- vectorOf
        (fromIntegral bracketFormulas)
        (choose (1, fromIntegral formulas))
      `suchThat` (listNoDuplicate . (++ serialsOfWrong))
    pseudoFormulas <- genPseudoList serialsOfWrong serialsOfBracket treeList `suchThat` noSimilarFormulas
    return $ LegalPropositionInst
        { serialsOfWrong = fromList serialsOfWrong
        , pseudoFormulas = pseudoFormulas
        }



genPseudoList :: [Int] -> [Int] -> [SynTree BinOp Char] -> Gen [String]
genPseudoList serialsOfWrong serialsOfBracket trees =
    let pointedTrees = zip [1..] trees
    in
        mapM (\(point, tree) -> if point `elem` serialsOfWrong
            then illegalDisplay tree
            else if point `elem` serialsOfBracket
                then bracketDisplay tree
                else legalDisplay tree) pointedTrees



legalDisplay :: SynTree BinOp Char -> Gen String
legalDisplay syntaxTree = return (display syntaxTree)



noSimilarFormulas :: [String] -> Bool
noSimilarFormulas pseudoFormulas = let pseudoFormulas' = map replace pseudoFormulas in  listNoDuplicate pseudoFormulas'



replace :: String -> String
replace = map judgeAndChange



judgeAndChange :: Char -> Char
judgeAndChange symbol = if isLetter symbol then '_' else symbol
