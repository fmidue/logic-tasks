{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module LegalPropositionSpec (spec) where

import Data.Set (toList, singleton)
import Data.Either (isLeft, isRight)
import Data.List ((\\))
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, choose, forAll, suchThat)

import Tasks.LegalProposition.Config (LegalPropositionConfig (..), LegalPropositionInst(..))
import Tasks.LegalProposition.PrintIllegal (illegalDisplay)
import Tasks.LegalProposition.PrintBracket (bracketDisplay,)
import Tasks.LegalProposition.Quiz (generateLegalPropositionInst, feedback)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Trees.Parsing (formulaParse)
import Tasks.SubTree.Parsing (subFormulasStringParse)
import Trees.Generate (genSynTree)
import Trees.Helpers (maxLeavesForNodes)
import SynTreeSpec (validBoundsSynTree)
import Trees.Print (display)
import TestHelpers (deleteBrackets, deleteSpaces, transferSetIntToString)

validBoundsLegalProposition :: Gen LegalPropositionConfig
validBoundsLegalProposition = do
    syntaxTreeConfig@SynTreeConfig {..}  <- validBoundsSynTree `suchThat` ((3 <=) . minNodes)
    let leaves = maxLeavesForNodes maxNodes
    formulas <- choose (1, min 15 $ if allowArrowOperators then 4 else 2 ^ (maxNodes - leaves))
    illegals <- choose (0, formulas)
    bracketFormulas <- choose (0, formulas - illegals)
    return $ LegalPropositionConfig
        {
            syntaxTreeConfig
            , formulas
            , illegals
            , bracketFormulas
        }

invalidBoundsLegalProposition :: Gen LegalPropositionConfig
invalidBoundsLegalProposition = do
    syntaxTreeConfig <- validBoundsSynTree
    formulas <- choose (1, 19)
    illegals <- choose (formulas + 1, 20)
    bracketFormulas <- choose (1, 20)
    return $ LegalPropositionConfig
        {
            syntaxTreeConfig
            , formulas
            , illegals
            , bracketFormulas
        }

illegalTest :: [Int] -> [String] -> Bool
illegalTest xs strings = all (\ x -> isLeft (formulaParse (strings !! (x - 1)))) xs

legalTest :: [Int] -> [String] -> Bool
legalTest xs strings = all (\ x -> isRight (formulaParse (strings !! (x - 1)))) xs

spec :: Spec
spec = do
    describe "illegalDisplay" $ do
        it "at least creates actual formula symbols" $
            forAll validBoundsSynTree $ \SynTreeConfig {..} ->
                forAll
                  (genSynTree
                    (minNodes, maxNodes)
                    maxDepth
                    usedLiterals
                    atLeastOccurring
                    allowArrowOperators
                    maxConsecutiveNegations
                  ) $ \synTree ->
                      forAll (deleteSpaces <$> illegalDisplay synTree) $ \str ->
                        subFormulasStringParse ("{" ++ str ++ "}") == Right (singleton str)
        it "the String after illegalDisplay can not parse " $
            forAll validBoundsSynTree $ \SynTreeConfig {..} ->
                forAll
                  (genSynTree
                    (minNodes, maxNodes)
                    maxDepth
                    usedLiterals
                    atLeastOccurring
                    allowArrowOperators
                    maxConsecutiveNegations
                  ) $ \synTree ->
                      forAll (illegalDisplay synTree) $ \str -> isLeft (formulaParse str)
    describe "bracket display" $ do
        it "the String after bracketDisplay just add a bracket " $
            forAll validBoundsSynTree $ \SynTreeConfig {..} ->
                forAll
                  (genSynTree
                    (minNodes, maxNodes)
                    maxDepth
                    usedLiterals
                    atLeastOccurring
                    allowArrowOperators
                    maxConsecutiveNegations
                  ) $ \synTree ->
                      forAll (bracketDisplay synTree) $ \str -> length str == length (display synTree) + 2
        it "the String can be parsed by formulaParse" $
            forAll validBoundsSynTree $ \SynTreeConfig {..} ->
                forAll
                  (genSynTree
                    (minNodes, maxNodes)
                    maxDepth
                    usedLiterals
                    atLeastOccurring
                    allowArrowOperators
                    maxConsecutiveNegations
                  ) $ \synTree ->
                      forAll (bracketDisplay synTree) $ \str -> formulaParse str == Right synTree
        it "the String remove all brackets should same with display remove all brackets" $
            forAll validBoundsSynTree $ \SynTreeConfig {..} ->
                forAll
                  (genSynTree
                    (minNodes, maxNodes)
                    maxDepth
                    usedLiterals
                    atLeastOccurring
                    allowArrowOperators
                    maxConsecutiveNegations
                  ) $ \synTree ->
                      forAll (bracketDisplay synTree) $ \str -> deleteBrackets str == deleteBrackets (display synTree)
    describe "generateLegalPropositionInst" $ do
        it "the generateLegalPropositionInst should generate expected illegal number" $
            forAll validBoundsLegalProposition $ \lPConfig ->
                forAll (generateLegalPropositionInst lPConfig) $ \LegalPropositionInst{..} ->
                  illegalTest (toList serialsOfWrong) pseudoFormulas
        it "the generateLegalPropositionInst should generate expected legal number" $
            forAll validBoundsLegalProposition $ \lPConfig@LegalPropositionConfig{..} ->
                forAll (generateLegalPropositionInst lPConfig) $ \LegalPropositionInst{..} ->
                  legalTest ([1.. (fromIntegral formulas)] \\ toList serialsOfWrong) pseudoFormulas
        it "the feedback designed for Instance can works good" $
            forAll validBoundsLegalProposition $ \lPConfig ->
                forAll (generateLegalPropositionInst lPConfig) $ \lPInst@LegalPropositionInst{..} ->
                  feedback lPInst (transferSetIntToString serialsOfWrong)
