{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module LegalNormalFormSpec (spec) where

import Data.Set (toList)
import Data.Either(isLeft, isRight)
import Test.Hspec (Spec, describe, it, xit)
import Test.QuickCheck (Gen, choose, forAll, suchThat, sublistOf, elements, ioProperty, withMaxSuccess, within)
import Data.List((\\))

import ParsingHelpers (fully)
import Formula.Types (Cnf, genCnf, genDnf, Dnf)
import Formula.Parsing (parser)
import Text.ParserCombinators.Parsec (ParseError, parse)
import Config (NormalFormConfig(..), BaseConfig(..))
import Trees.Types (SynTree(..), BinOp(..))
import Trees.Helpers (cnfToSynTree, dnfToSynTree)
import Tasks.LegalNormalForm.Config (LegalNormalFormConfig(..), LegalNormalFormInst(..), checkLegalNormalFormConfig)
import Tasks.LegalNormalForm.GenerateIllegal (genIllegalCnfSynTree, genIllegalDnfSynTree, )
import Tasks.LegalNormalForm.Quiz (generateLegalCNFInst, generateLegalDNFInst)
import Control.OutputCapable.Blocks (Language(German))
import Control.OutputCapable.Blocks.Debug(checkConfigWith)

import FormulaSpec (validBoundsCnf)

validBoundsLegalNormalForm :: Gen LegalNormalFormConfig
validBoundsLegalNormalForm = do
    ((minClauseAmount,maxClauseAmount),(minClauseLength,maxClauseLength),usedAtoms) <- validBoundsCnf

    let
      maxFormulas = (maxClauseLength - minClauseLength + 1) ^ (maxClauseAmount - minClauseAmount + 1) `div` 2 + 1
      formulaUpperBound
        | maxFormulas < 0 = 15 -- Int overflow
        | otherwise = min 15 maxFormulas

    formulas <- choose
      (1, formulaUpperBound)
    illegals <- choose (0, formulas)
    let includeFormWithJustOneClause = minClauseAmount == 1 && formulas - illegals > 0
        includeFormWithJustOneLiteralPerClause = minClauseLength == 1 && formulas - illegals > 1
    allowArrowOperators <- elements [True, False]
    return $ LegalNormalFormConfig
        {
          normalFormConfig = NormalFormConfig{
            minClauseAmount,
            maxClauseAmount,
            baseConf = BaseConfig{
                minClauseLength,
                maxClauseLength,
                usedAtoms
            }
          },
          formulas,
          illegals,
          includeFormWithJustOneClause,
          includeFormWithJustOneLiteralPerClause,
          maxStringSize =  maxClauseAmount * (maxClauseLength * 6 + 5),
          minStringSize = minClauseAmount * ((minClauseLength - 1) * 4 + 1),
          allowArrowOperators,
          printSolution = False,
          extraText = Nothing
        }

invalidBoundsLegalCNF :: Gen LegalNormalFormConfig
invalidBoundsLegalCNF = do
    usedAtoms <- sublistOf ['A' .. 'Z'] `suchThat` \atoms -> not (null atoms) && (10>=length atoms)
    maxClauseLength <- choose (1, 2 * length usedAtoms)
    minClauseLength <- choose (maxClauseLength, 100)
    let clauses = product (take maxClauseLength (reverse [1 .. (2 * length usedAtoms)]))
    maxClauseAmount <- choose (1, max 15 clauses)  `suchThat` \amount ->amount > 1 || maxClauseLength > 1
    minClauseAmount <- choose (1, maxClauseAmount + 20)
    formulas <- choose (-10, max 15 (maxClauseLength - minClauseLength + 1) ^ (maxClauseAmount - minClauseAmount + 1))
    illegals <- choose (-5, -1)
    minStringSize <- choose (minClauseLength * (2 + minClauseAmount), 300)
    maxStringSize <- choose (1, minStringSize)
    return $ LegalNormalFormConfig
        {
          normalFormConfig = NormalFormConfig{
            minClauseAmount,
            maxClauseAmount,
            baseConf = BaseConfig{
                minClauseLength,
                maxClauseLength,
                usedAtoms
            }
          },
          formulas,
          illegals,
          includeFormWithJustOneClause = True,
          includeFormWithJustOneLiteralPerClause = False,
          maxStringSize,
          minStringSize,
          allowArrowOperators = False,
          printSolution = False,
          extraText = Nothing
        }

timeout :: Int
timeout = 30000000 -- 30 seconds

spec :: Spec
spec = do
    describe "validBoundsLegalNormalForm" $
        it "produces a valid config" $
          withMaxSuccess 1000 $ forAll validBoundsLegalNormalForm $ \conf ->
            ioProperty $ checkConfigWith German conf checkLegalNormalFormConfig

    describe "invalidBoundsLegalCNF" $
        xit "produces a valid config" $
          forAll invalidBoundsLegalCNF $ \conf ->
            ioProperty (not <$> checkConfigWith German conf checkLegalNormalFormConfig)

    describe "genIllegalCnfSynTree" $
        it "the syntax Tree are not CNF syntax tree" $
            forAll validBoundsLegalNormalForm $
              \LegalNormalFormConfig {normalFormConfig = NormalFormConfig{baseConf = BaseConfig{..}, ..}, ..} ->
                forAll
                  (genIllegalCnfSynTree
                    (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength)
                     usedAtoms
                     allowArrowOperators
                  )
                  (not . judgeCnfSynTree)
    describe "genIllegalDnfSynTree" $
        it "the syntax Tree are not DNF syntax tree" $
            forAll validBoundsLegalNormalForm $
              \LegalNormalFormConfig {normalFormConfig = NormalFormConfig{baseConf = BaseConfig{..}, ..}, ..} ->
                forAll
                  (genIllegalDnfSynTree
                    (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength)
                     usedAtoms
                     allowArrowOperators
                  )
                  (not . judgeDnfSynTree)
    describe "judgeCnfSynTree" $
        it "is reasonably implemented" $
            forAll validBoundsLegalNormalForm $
              \LegalNormalFormConfig {normalFormConfig = NormalFormConfig{baseConf = BaseConfig{..}, ..}} ->
                forAll
                  (genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedAtoms False)
                  (judgeCnfSynTree . cnfToSynTree)
    describe "judgeDnfSynTree" $
        it "is reasonably implemented" $
            forAll validBoundsLegalNormalForm $
              \LegalNormalFormConfig {normalFormConfig = NormalFormConfig{baseConf = BaseConfig{..}, ..}} ->
                forAll
                  (genDnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedAtoms False)
                  (judgeDnfSynTree . dnfToSynTree)
    describe "generateLegalCNFInst" $ do
        it "all of the formulas in the wrong serial should not be Cnf" $
            within timeout $ forAll validBoundsLegalNormalForm $ \config ->
                forAll (generateLegalCNFInst config) $ \LegalNormalFormInst{..} ->
                  all (\x -> isLeft (cnfParse (formulaStrings !! (x - 1)))) serialsOfWrong
        it "all of the formulas not in the wrong serial should be Cnf" $
            within timeout $ forAll validBoundsLegalNormalForm $ \config@LegalNormalFormConfig{..} ->
                forAll (generateLegalCNFInst config) $ \LegalNormalFormInst{..} ->
                  all (\x -> isRight (cnfParse (formulaStrings !! (x - 1)))) ([1..formulas] \\ toList serialsOfWrong)
    describe "generateLegalDNFInst" $ do
        it "all of the formulas in the wrong serial should not be Dnf" $
            within timeout $ forAll validBoundsLegalNormalForm $ \config ->
                forAll (generateLegalDNFInst config) $ \LegalNormalFormInst{..} ->
                  all (\x -> isLeft (dnfParse (formulaStrings !! (x - 1)))) serialsOfWrong
        it "all of the formulas not in the wrong serial should be Dnf" $
            within timeout $ forAll validBoundsLegalNormalForm $ \config@LegalNormalFormConfig{..} ->
                forAll (generateLegalDNFInst config) $ \LegalNormalFormInst{..} ->
                  all (\x -> isRight (dnfParse (formulaStrings !! (x - 1)))) ([1..formulas] \\ toList serialsOfWrong)

judgeCnfSynTree :: SynTree BinOp a -> Bool
judgeCnfSynTree (Binary And a b) = judgeCnfSynTree a && judgeCnfSynTree b
judgeCnfSynTree (Binary Or a b) =  judgeCnfOr a && judgeCnfOr b
judgeCnfSynTree (Not a) = judgeLeaf a
judgeCnfSynTree (Leaf _) = True
judgeCnfSynTree _ = False

judgeDnfSynTree :: SynTree BinOp a -> Bool
judgeDnfSynTree (Binary Or a b) = judgeDnfSynTree a && judgeDnfSynTree b
judgeDnfSynTree (Binary And a b) =  judgeDnfAnd a && judgeDnfAnd b
judgeDnfSynTree (Not a) = judgeLeaf a
judgeDnfSynTree (Leaf _) = True
judgeDnfSynTree _ = False

judgeCnfOr :: SynTree BinOp a -> Bool
judgeCnfOr (Binary Or a b) =  judgeCnfOr a && judgeCnfOr b
judgeCnfOr (Not a) = judgeLeaf a
judgeCnfOr (Leaf _) = True
judgeCnfOr _ = False

judgeDnfAnd :: SynTree BinOp a -> Bool
judgeDnfAnd (Binary And a b) =  judgeDnfAnd a && judgeDnfAnd b
judgeDnfAnd (Not a) = judgeLeaf a
judgeDnfAnd (Leaf _) = True
judgeDnfAnd _ = False

judgeLeaf :: SynTree o a -> Bool
judgeLeaf (Leaf _) = True
judgeLeaf _ = False

cnfParse :: String -> Either ParseError Cnf
cnfParse = parse (fully parser) ""

dnfParse :: String -> Either ParseError Dnf
dnfParse = parse (fully parser) ""
