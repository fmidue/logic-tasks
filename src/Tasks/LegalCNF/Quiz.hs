{-# LANGUAGE RecordWildCards #-}

module Tasks.LegalCNF.Quiz (
    generateLegalCNFInst
    ) where


import Formula.Types (genCnf)

import Data.List ((\\))
import Data.Set (fromList)
import Test.QuickCheck (Gen, choose, elements, suchThat, vectorOf)

import Auxiliary (listNoDuplicate)
import Config (BaseConfig(..), CnfConfig(..))
import Tasks.LegalCNF.Config (LegalCNFConfig(..), LegalCNFInst(..))
import Tasks.LegalCNF.GenerateIllegal (genIllegalSynTree)
import Trees.Helpers (cnfToSynTree)
import Trees.Print (simplestDisplay)
import Trees.Types (BinOp(..), SynTree(..))




generateLegalCNFInst :: LegalCNFConfig -> Gen LegalCNFInst
generateLegalCNFInst config@LegalCNFConfig {..} = do
    serialsOfWrong <- vectorOf illegals (choose (1, formulas) )`suchThat` listNoDuplicate
    let serial1 = [1.. formulas] \\ serialsOfWrong
    serialsOfJustOneClause <- vectorOf (if includeFormWithJustOneClause then 1 else 0) (elements serial1)
    let serial2 = serial1 \\ serialsOfJustOneClause
    serialsOfJustOneLiteralPerClause <- vectorOf
        (if includeFormWithJustOneLiteralPerClause then 1 else 0)
        (elements serial2)
    treeList <- genSynTreeList
        serialsOfWrong
        serialsOfJustOneClause
        serialsOfJustOneLiteralPerClause
        [1 .. formulas]
        config
      `suchThat` (listNoDuplicate . map (simplestDisplay . fmap (const '_')))
    return $ LegalCNFInst { serialsOfWrong = fromList serialsOfWrong
                          , formulaStrings = map simplestDisplay treeList
                          , showSolution = printSolution
                          , addText = extraText}



genSynTreeList :: [Int] -> [Int] -> [Int] -> [Int] -> LegalCNFConfig -> Gen [SynTree BinOp Char]
genSynTreeList
  serialsOfWrong
  serialsOfJustOneClause
  serialsOfJustOneLiteralPerClause
  formulasList
  config@LegalCNFConfig{..} =
    mapM (\serial ->
      genSynTreeWithSerial
        serialsOfWrong
        serialsOfJustOneClause
        serialsOfJustOneLiteralPerClause
        config
        serial
      `suchThat` checkSize minStringSize maxStringSize) formulasList



genSynTreeWithSerial :: [Int] -> [Int] -> [Int] -> LegalCNFConfig -> Int -> Gen (SynTree BinOp Char)
genSynTreeWithSerial
  serialsOfWrong
  serialsOfJustOneClause
  serialsOfJustOneLiteralPerClause
  LegalCNFConfig {cnfConfig = CnfConfig {baseConf = BaseConfig{..},..},..}
  serial
    | serial `elem` serialsOfWrong =
        genIllegalSynTree
          (minClauseAmount, maxClauseAmount)
          (minClauseLength, maxClauseLength)
          usedLiterals
          allowArrowOperators
    | serial `elem` serialsOfJustOneClause =
        cnfToSynTree <$>
       genCnf (1, 1) (minClauseLength, maxClauseLength) usedLiterals False
    | serial `elem` serialsOfJustOneLiteralPerClause =
        cnfToSynTree <$>
       genCnf (minClauseAmount, maxClauseAmount) (1, 1) usedLiterals False
    | otherwise =
        cnfToSynTree <$>
       genCnf
          (minClauseAmount, maxClauseAmount)
          (minClauseLength, maxClauseLength)
          usedLiterals
          False



checkSize :: Int -> Int -> SynTree BinOp Char -> Bool
checkSize minStringSize maxStringSize synTree =
  let
    stringLength = length (simplestDisplay synTree)
  in
    stringLength <= maxStringSize && stringLength >= minStringSize
