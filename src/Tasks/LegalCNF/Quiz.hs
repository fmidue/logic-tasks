{-# LANGUAGE RecordWildCards #-}

module Tasks.LegalCNF.Quiz (
    generateLegalCNFInst
    ) where


import qualified Formula.Types (genCnf)
import qualified Tasks.LegalCNF.GenerateLegal (genCnf)

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
    serialsOfExternal <- vectorOf externalGenFormulas (elements serial1 ) `suchThat` listNoDuplicate
    let serial2 = serial1 \\ serialsOfExternal
    serialsOfJustOneClause <- vectorOf (if includeFormWithJustOneClause then 1 else 0) (elements serial2)
    let serial3 = serial2 \\ serialsOfJustOneClause
    serialsOfJustOneLiteralPerClause <- vectorOf
        (if includeFormWithJustOneLiteralPerClause then 1 else 0)
        (elements serial3)
    treeList <- genSynTreeList
        serialsOfWrong
        serialsOfExternal
        serialsOfJustOneClause
        serialsOfJustOneLiteralPerClause
        [1 .. formulas]
        config
      `suchThat` (listNoDuplicate . map (simplestDisplay . fmap (const '_')))
    return $ LegalCNFInst { serialsOfWrong = fromList serialsOfWrong
                          , formulaStrings = map simplestDisplay treeList
                          , showSolution = printSolution
                          , addText = extraText}



genSynTreeList :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> LegalCNFConfig -> Gen [SynTree BinOp Char]
genSynTreeList
  serialsOfWrong
  serialsOfExternal
  serialsOfJustOneClause
  serialsOfJustOneLiteralPerClause
  formulasList
  config@LegalCNFConfig{..} =
    mapM (\serial ->
      genSynTreeWithSerial
        serialsOfWrong
        serialsOfExternal
        serialsOfJustOneClause
        serialsOfJustOneLiteralPerClause
        config
        serial
      `suchThat` checkSize minStringSize maxStringSize) formulasList



genSynTreeWithSerial :: [Int] -> [Int] -> [Int] -> [Int] -> LegalCNFConfig -> Int -> Gen (SynTree BinOp Char)
genSynTreeWithSerial
  serialsOfWrong
  serialsOfExternal
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
    | serial `elem` serialsOfExternal =
        cnfToSynTree <$>
        Formula.Types.genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals
    | serial `elem` serialsOfJustOneClause =
        cnfToSynTree <$>
        Tasks.LegalCNF.GenerateLegal.genCnf (1, 1) (minClauseLength, maxClauseLength) usedLiterals
    | serial `elem` serialsOfJustOneLiteralPerClause =
        cnfToSynTree <$>
        Tasks.LegalCNF.GenerateLegal.genCnf (minClauseAmount, maxClauseAmount) (1, 1) usedLiterals
    | otherwise =
        cnfToSynTree <$>
        Tasks.LegalCNF.GenerateLegal.genCnf
          (minClauseAmount, maxClauseAmount)
          (minClauseLength, maxClauseLength)
          usedLiterals



checkSize :: Int -> Int -> SynTree BinOp Char -> Bool
checkSize minStringSize maxStringSize synTree =
  let
    stringLength = length (simplestDisplay synTree)
  in
    stringLength <= maxStringSize && stringLength >= minStringSize
