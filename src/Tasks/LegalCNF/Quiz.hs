{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}


module Tasks.LegalCNF.Quiz  (
    generateLegalCNFInst,
    feedback,
) where

import Tasks.LegalCNF.Config (LegalCNFInst(..), LegalCNFConfig(..))
import Config(CnfConfig(..), BaseConfig(..))
import Test.QuickCheck (Gen, suchThat, choose, vectorOf, elements)
import Trees.Types (SynTree(..), BinOp(..))
import Tasks.LegalCNF.GenerateIllegal (genIllegalSynTree, )
import qualified Tasks.LegalCNF.GenerateLegal (genCnf)
import Trees.Print(simplestDisplay)
import Tasks.LegalProposition.Parsing (illegalPropositionStringParse)
import Data.Set (fromList)
import Auxiliary (listNoDuplicate)
import Data.List ( (\\) )
import Trees.Helpers (cnfToSynTree)
import qualified Formula.Types (genCnf)

generateLegalCNFInst :: LegalCNFConfig -> Gen LegalCNFInst
generateLegalCNFInst lCConfig@LegalCNFConfig {..} = do
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
        lCConfig
      `suchThat` (listNoDuplicate . map (simplestDisplay . fmap (const '_')))
    return $ LegalCNFInst {serialsOfWrong = fromList serialsOfWrong, formulaStrings = map simplestDisplay treeList}


genSynTreeList :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> LegalCNFConfig -> Gen [SynTree BinOp Char]
genSynTreeList
  serialsOfWrong
  serialsOfExternal
  serialsOfJustOneClause
  serialsOfJustOneLiteralPerClause
  formulasList
  lCConfig@LegalCNFConfig{..} =
    mapM (\serial ->
      genSynTreeWithSerial
        serialsOfWrong
        serialsOfExternal
        serialsOfJustOneClause
        serialsOfJustOneLiteralPerClause
        lCConfig
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

feedback :: LegalCNFInst -> String -> Bool
feedback LegalCNFInst {serialsOfWrong} input = illegalPropositionStringParse input == Right serialsOfWrong
