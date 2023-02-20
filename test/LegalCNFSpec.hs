{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module LegalCNFSpec (spec) where

import Data.Set (toList)
import Data.Either(isLeft, isRight)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, choose, forAll, suchThat, sublistOf, elements)
import Data.List((\\))

import ParsingHelpers (whitespace)
import Formula.Types (Cnf, lengthBound)
import Formula.Parsing (parser)
import Text.ParserCombinators.Parsec (ParseError, eof, parse)
import Config (CnfConfig(..), BaseConfig(..))
import Trees.Types (SynTree(..), BinOp(..))
import Trees.Helpers (cnfToSynTree)
import Tasks.LegalCNF.Config (LegalCNFConfig(..), LegalCNFInst(..))
import Tasks.LegalCNF.GenerateIllegal (genIllegalSynTree, )
import Tasks.LegalCNF.GenerateLegal (genCnf)
import Tasks.LegalCNF.Quiz (generateLegalCNFInst)
import Tasks.LegalProposition.Parsing (illegalPropositionStringParse)
import TestHelpers (transferSetIntToString)

validBoundsLegalCNF :: Gen LegalCNFConfig
validBoundsLegalCNF = do
    usedLiterals <- sublistOf ['A' .. 'Z'] `suchThat` (not . null)
    maxClauseLength <- choose (1, min 12 (length usedLiterals))
    minClauseLength <- choose (1, maxClauseLength)
    let clauses = min 12 (product (take maxClauseLength (reverse [1 .. (length usedLiterals)])))
        minClauses = lengthBound minClauseLength (length usedLiterals) (minClauseLength, maxClauseLength)
    maxClauseAmount <- choose (1, clauses)  `suchThat` \amount -> amount > 1 || maxClauseLength > 1
    minClauseAmount <- choose (1, min minClauses maxClauseAmount)
    formulas <- choose
      (1, min 15 ((maxClauseLength - minClauseLength + 1) ^ (maxClauseAmount - minClauseAmount + 1) `div` 2 + 1))
    illegals <- choose (0, formulas)
    externalGenFormulas <- choose (0, formulas - illegals)
    let includeFormWithJustOneClause = minClauseAmount == 1 && formulas - illegals - externalGenFormulas > 0
        includeFormWithJustOneLiteralPerClause = minClauseLength == 1 && formulas - illegals - externalGenFormulas > 1
    allowArrowOperators <- elements [True, False]
    return $ LegalCNFConfig
        {
          cnfConfig = CnfConfig{
            minClauseAmount,
            maxClauseAmount,
            baseConf = BaseConfig{
                minClauseLength,
                maxClauseLength,
                usedLiterals
            }
          },
          formulas,
          illegals,
          includeFormWithJustOneClause,
          includeFormWithJustOneLiteralPerClause,
          externalGenFormulas,
          maxStringSize =  maxClauseAmount * (maxClauseLength * 6 + 5),
          minStringSize = minClauseAmount * ((minClauseLength - 1) * 5 + 1),
          allowArrowOperators
        }

invalidBoundsLegalCNF :: Gen LegalCNFConfig
invalidBoundsLegalCNF = do
    usedLiterals <- sublistOf ['A' .. 'Z'] `suchThat` \literals -> not (null literals) && (10>=length literals)
    maxClauseLength <- choose (1, 2 * length usedLiterals)
    minClauseLength <- choose (maxClauseLength, 100)
    let clauses = product (take maxClauseLength (reverse [1 .. (2 * length usedLiterals)]))
    maxClauseAmount <- choose (1, max 15 clauses)  `suchThat` \amount ->amount > 1 || maxClauseLength > 1
    minClauseAmount <- choose (1, maxClauseAmount + 20)
    formulas <- choose (-10, max 15 (maxClauseLength - minClauseLength + 1) ^ (maxClauseAmount - minClauseAmount + 1))
    illegals <- choose (-5, -1)
    externalGenFormulas <- choose (-10, 100)
    minStringSize <- choose (minClauseLength * (2 + minClauseAmount), 300)
    maxStringSize <- choose (1, minStringSize)
    return $ LegalCNFConfig
        {
          cnfConfig = CnfConfig{
            minClauseAmount,
            maxClauseAmount,
            baseConf = BaseConfig{
                minClauseLength,
                maxClauseLength,
                usedLiterals
            }
          },
          formulas,
          illegals,
          includeFormWithJustOneClause = True,
          includeFormWithJustOneLiteralPerClause = False,
          externalGenFormulas,
          maxStringSize,
          minStringSize,
          allowArrowOperators = False
        }

illegalTest :: [Int] -> [String] -> Bool
illegalTest xs strings = all (\ x -> isLeft (cnfParse (strings !! (x - 1)))) xs

legalTest :: [Int] -> [String] -> Bool
legalTest xs strings = all (\ x -> isRight (cnfParse (strings !! (x - 1)))) xs

spec :: Spec
spec = do
    describe "genIllegalSynTree" $
        it "the syntax Tree are not CNF syntax tree" $
            forAll validBoundsLegalCNF $ \LegalCNFConfig {cnfConfig = CnfConfig{baseConf = BaseConfig{..}, ..}, ..} ->
                forAll
                  (genIllegalSynTree
                    (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength)
                     usedLiterals
                     allowArrowOperators
                  )
                  (not . judgeCnfSynTree)
    describe "judgeCNFSynTree" $
        it "is reasonably implemented" $
            forAll validBoundsLegalCNF $ \LegalCNFConfig {cnfConfig = CnfConfig{baseConf = BaseConfig{..}, ..}} ->
                forAll
                  (genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals)
                  (judgeCnfSynTree . cnfToSynTree)
    describe "generateLegalCNFInst" $
        it "all of the formulas in the wrong serial should not be Cnf" $
            forAll validBoundsLegalCNF $ \config ->
                forAll (generateLegalCNFInst config) $ \LegalCNFInst{..} ->
                  illegalTest (toList serialsOfWrong) formulaStrings
    describe "generateLegalCNFInst" $ do
        it "all of the formulas not in the wrong serial should be Cnf" $
            forAll validBoundsLegalCNF $ \config@LegalCNFConfig{..} ->
                forAll (generateLegalCNFInst config) $ \LegalCNFInst{..} ->
                  legalTest ([1..formulas] \\ toList serialsOfWrong) formulaStrings
        it "the feedback designed for Instance works as expected" $
            forAll validBoundsLegalCNF $ \config ->
                forAll (generateLegalCNFInst config) $ \LegalCNFInst{..} ->
                  illegalPropositionStringParse(transferSetIntToString serialsOfWrong) == Right serialsOfWrong

judgeCnfSynTree :: SynTree BinOp a -> Bool
judgeCnfSynTree (Binary And a b) = judgeCnfSynTree a && judgeCnfSynTree b
judgeCnfSynTree (Binary Or a b) =  judgeCnfOr a && judgeCnfOr b
judgeCnfSynTree (Not a) = judgeLeaf a
judgeCnfSynTree (Leaf _) = True
judgeCnfSynTree _ = False

judgeCnfOr :: SynTree BinOp a -> Bool
judgeCnfOr (Binary Or a b) =  judgeCnfOr a && judgeCnfOr b
judgeCnfOr (Not a) = judgeLeaf a
judgeCnfOr (Leaf _) = True
judgeCnfOr _ = False

judgeLeaf :: SynTree o a -> Bool
judgeLeaf (Leaf _) = True
judgeLeaf _ = False

cnfParse :: String -> Either ParseError Cnf
cnfParse = parse (whitespace >> parser <* eof) ""
