{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Tasks.LegalProposition.Quiz (
    generateLegalPropositionInst,
    ) where


import Data.Char (isLetter)
import Test.QuickCheck (Gen, choose, suchThat, vectorOf)

import Auxiliary (listNoDuplicate)
import Tasks.LegalProposition.Config (
  LegalPropositionConfig (..),
  LegalPropositionInst (..),
  PropErrorReason(..),
  PropFormulaInfo (..),
  )
import Tasks.LegalProposition.PrintBracket (bracketDisplay)
import Tasks.LegalProposition.PrintIllegal (illegalDisplay)
import Trees.Generate (genSynTree)
import Trees.Helpers (similarExist)
import Trees.Print (display)
import Trees.Types (BinOp, SynTree)
import Data.Bifunctor (Bifunctor(second))




generateLegalPropositionInst :: LegalPropositionConfig -> Gen LegalPropositionInst
generateLegalPropositionInst LegalPropositionConfig  {..} = do
    treeList <- vectorOf
        (fromIntegral formulas)
        (genSynTree syntaxTreeConfig)
      `suchThat` (not . similarExist)
    serialsOfWrong <- vectorOf (fromIntegral illegals) (choose (1, fromIntegral formulas) )`suchThat` listNoDuplicate
    serialsOfBracket <- vectorOf
        (fromIntegral bracketFormulas)
        (choose (1, fromIntegral formulas))
      `suchThat` (listNoDuplicate . (++ serialsOfWrong))
    pseudoFormulas <- genPseudoList serialsOfWrong serialsOfBracket treeList `suchThat` (noSimilarFormulas . map fst)
    return $ LegalPropositionInst
        { formulaInfos =
            zipWith
              (curry (\(i,(d,errorReason)) -> (i,maybe (Correct (treeList !! (i - 1))) Erroneous errorReason,d)))
              [1..]
              pseudoFormulas
        , showSolution = printSolution
        , addText = extraText
        }



genPseudoList :: [Int] -> [Int] -> [SynTree BinOp Char] -> Gen [(String, Maybe PropErrorReason)]
genPseudoList serialsOfWrong serialsOfBracket trees =
    let pointedTrees = zip [1..] trees
    in
        mapM (\(point, tree) -> if point `elem` serialsOfWrong
            then second Just <$> illegalDisplay tree
            else if point `elem` serialsOfBracket
                then (,Nothing) <$> bracketDisplay tree
                else (,Nothing) <$> legalDisplay tree) pointedTrees



legalDisplay :: SynTree BinOp Char -> Gen String
legalDisplay syntaxTree = return (display syntaxTree)



noSimilarFormulas :: [String] -> Bool
noSimilarFormulas pseudoFormulas = let pseudoFormulas' = map replace pseudoFormulas in  listNoDuplicate pseudoFormulas'



replace :: String -> String
replace = map judgeAndChange



judgeAndChange :: Char -> Char
judgeAndChange symbol = if isLetter symbol then '_' else symbol
