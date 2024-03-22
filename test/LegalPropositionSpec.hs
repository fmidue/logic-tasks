{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module LegalPropositionSpec (spec) where

import Data.Set (toList)
import Data.Either (isLeft, isRight)
import Data.List ((\\))
import Data.Char (isLetter)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, choose, forAll, suchThat)

import Tasks.LegalProposition.Config (LegalPropositionConfig (..), LegalPropositionInst(..))
import Tasks.LegalProposition.PrintIllegal (illegalDisplay)
import Tasks.LegalProposition.PrintBracket (bracketDisplay,)
import Tasks.LegalProposition.Quiz (generateLegalPropositionInst)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Trees.Parsing (formulaParse)
import Trees.Generate (genSynTree)
import Trees.Helpers (maxLeavesForNodes)
import SynTreeSpec (validBoundsSynTree)
import Trees.Print (display)
import TestHelpers (deleteBrackets, deleteSpaces)

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
            , extraText = Nothing
            , printSolution = False
        }

spec :: Spec
spec = do
    describe "illegalDisplay" $ do
        it "at least creates actual formula symbols" $
            forAll validBoundsSynTree $ \synTreeConfig@SynTreeConfig {..} ->
                forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (deleteSpaces <$> illegalDisplay synTree) $
                      all (\c -> c `elem` "()∧∨¬<=>" || isLetter c)
        it "the string after illegalDisplay cannot be parsed" $
            forAll validBoundsSynTree $ \synTreeConfig@SynTreeConfig {..} ->
                forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (illegalDisplay synTree) $ \str -> isLeft (formulaParse str)
    describe "bracket display" $ do
        it "the String after bracketDisplay just add a bracket " $
            forAll validBoundsSynTree $ \synTreeConfig@SynTreeConfig {..} ->
                forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (bracketDisplay synTree) $ \str -> length str == length (display synTree) + 2
        it "the String can be parsed by formulaParse" $
            forAll validBoundsSynTree $ \synTreeConfig@SynTreeConfig {..} ->
                forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (bracketDisplay synTree) $ \str -> formulaParse str == Right synTree
        it "the String remove all brackets should same with display remove all brackets" $
            forAll validBoundsSynTree $ \synTreeConfig@SynTreeConfig {..} ->
                forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (bracketDisplay synTree) $ \str -> deleteBrackets str == deleteBrackets (display synTree)
    describe "generateLegalPropositionInst" $ do
        it "the generateLegalPropositionInst should generate expected illegal number" $
            forAll validBoundsLegalProposition $ \config ->
                forAll (generateLegalPropositionInst config) $ \LegalPropositionInst{..} ->
                  all (\x -> isLeft (formulaParse (pseudoFormulas !! (x - 1)))) (toList serialsOfWrong)
        it "the generateLegalPropositionInst should generate expected legal number" $
            forAll validBoundsLegalProposition $ \config@LegalPropositionConfig{..} ->
                forAll (generateLegalPropositionInst config) $ \LegalPropositionInst{..} ->
                  all
                  (\x -> isRight (formulaParse (pseudoFormulas !! (x - 1))))
                  ([1 .. fromIntegral formulas] \\ toList serialsOfWrong)
