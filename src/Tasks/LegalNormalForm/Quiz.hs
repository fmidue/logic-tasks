{-# LANGUAGE RecordWildCards #-}

module Tasks.LegalNormalForm.Quiz
  ( generateLegalCNFInst,
    generateLegalDNFInst,
  )
where

import Auxiliary (listNoDuplicate)
import Config (BaseConfig (..), NormalFormConfig (..))
import Data.List ((\\))
import Data.Set (fromList)
import Formula.Types (genCnf, genDnf)
import Tasks.LegalNormalForm.Config (LegalNormalFormConfig (..), LegalNormalFormInst (..))
import Tasks.LegalNormalForm.GenerateIllegal (genIllegalCnfSynTree, genIllegalDnfSynTree)
import Test.QuickCheck (Gen, choose, elements, suchThat, vectorOf)
import Trees.Helpers (cnfToSynTree, dnfToSynTree)
import Trees.Print (simplestDisplay)
import Trees.Types (BinOp (..), SynTree (..))

generateLegalCNFInst :: LegalNormalFormConfig -> Gen LegalNormalFormInst
generateLegalCNFInst = generateLegalNormalFormInst genCnf cnfToSynTree genIllegalCnfSynTree

generateLegalDNFInst :: LegalNormalFormConfig -> Gen LegalNormalFormInst
generateLegalDNFInst = generateLegalNormalFormInst genDnf dnfToSynTree genIllegalDnfSynTree

generateLegalNormalFormInst ::
  ((Int, Int) -> (Int, Int) -> [Char] -> Bool -> Gen a) ->
  (a -> SynTree BinOp Char) ->
  ((Int,Int) -> (Int,Int) -> [Char] -> Bool -> Gen (SynTree BinOp Char)) ->
  LegalNormalFormConfig ->
  Gen LegalNormalFormInst
generateLegalNormalFormInst gen toSynTree genIllegal config@LegalNormalFormConfig {..} = do
  serialsOfWrong <- vectorOf illegals (choose (1, formulas)) `suchThat` listNoDuplicate
  let serial1 = [1 .. formulas] \\ serialsOfWrong
  serialsOfJustOneClause <- vectorOf (if includeFormWithJustOneClause then 1 else 0) (elements serial1)
  let serial2 = serial1 \\ serialsOfJustOneClause
  serialsOfJustOneLiteralPerClause <-
    vectorOf
      (if includeFormWithJustOneLiteralPerClause then 1 else 0)
      (elements serial2)
  treeList <-
    genSynTreeList
      serialsOfWrong
      serialsOfJustOneClause
      serialsOfJustOneLiteralPerClause
      [1 .. formulas]
      config
      gen
      toSynTree
      genIllegal
      `suchThat` (listNoDuplicate . map (simplestDisplay . fmap (const '_')))
  return $
    LegalNormalFormInst
      { serialsOfWrong = fromList serialsOfWrong,
        formulaStrings = map simplestDisplay treeList,
        showSolution = printSolution,
        addText = extraText
      }

genSynTreeList ::
  [Int] ->
  [Int] ->
  [Int] ->
  [Int] ->
  LegalNormalFormConfig ->
  ((Int, Int) -> (Int, Int) -> [Char] -> Bool -> Gen a) ->
  (a -> SynTree BinOp Char) ->
  ((Int,Int) -> (Int,Int) -> [Char] -> Bool -> Gen (SynTree BinOp Char)) ->
  Gen [SynTree BinOp Char]
genSynTreeList
  serialsOfWrong
  serialsOfJustOneClause
  serialsOfJustOneLiteralPerClause
  formulasList
  config@LegalNormalFormConfig {..}
  gen
  toSynTree
  genIllegal =
    mapM
      ( \serial ->
          genSynTreeWithSerial
            serialsOfWrong
            serialsOfJustOneClause
            serialsOfJustOneLiteralPerClause
            config
            serial
            gen
            toSynTree
            genIllegal
            `suchThat` checkSize minStringSize maxStringSize
      )
      formulasList

genSynTreeWithSerial ::
  [Int] ->
  [Int] ->
  [Int] ->
  LegalNormalFormConfig ->
  Int ->
  ((Int, Int) -> (Int, Int) -> [Char] -> Bool -> Gen a) ->
  (a -> SynTree BinOp Char) ->
  ((Int,Int) -> (Int,Int) -> [Char] -> Bool -> Gen (SynTree BinOp Char)) ->
  Gen (SynTree BinOp Char)
genSynTreeWithSerial
  serialsOfWrong
  serialsOfJustOneClause
  serialsOfJustOneLiteralPerClause
  LegalNormalFormConfig {normalFormConfig = NormalFormConfig {baseConf = BaseConfig {..}, ..}, ..}
  serial
  gen
  toSynTree
  genIllegal
    | serial `elem` serialsOfWrong =
        genIllegal
          (minClauseAmount, maxClauseAmount)
          (minClauseLength, maxClauseLength)
          usedAtoms
          allowArrowOperators
    | serial `elem` serialsOfJustOneClause =
        toSynTree
          <$> gen (1, 1) (minClauseLength, maxClauseLength) usedAtoms False
    | serial `elem` serialsOfJustOneLiteralPerClause =
        toSynTree
          <$> gen (minClauseAmount, maxClauseAmount) (1, 1) usedAtoms False
    | otherwise =
        toSynTree
          <$> gen
            (minClauseAmount, maxClauseAmount)
            (minClauseLength, maxClauseLength)
            usedAtoms
            False

checkSize :: Int -> Int -> SynTree BinOp Char -> Bool
checkSize minStringSize maxStringSize synTree =
  let stringLength = length (simplestDisplay synTree)
   in stringLength <= maxStringSize && stringLength >= minStringSize
