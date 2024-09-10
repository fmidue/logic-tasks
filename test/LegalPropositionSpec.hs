{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module LegalPropositionSpec (spec) where

import Data.Either (isLeft, isRight)
import Data.List ((\\))
import Data.Char (isLetter)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, choose, forAll, suchThat, within)

import Tasks.LegalProposition.Config (
  LegalPropositionConfig (..),
  LegalPropositionInst(..),
  checkLegalPropositionConfig,
  defaultLegalPropositionConfig)
import Tasks.LegalProposition.PrintIllegal (illegalDisplay)
import Tasks.LegalProposition.PrintBracket (bracketDisplay,)
import Tasks.LegalProposition.Quiz (generateLegalPropositionInst)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Trees.Parsing (formulaParse)
import Trees.Generate (genSynTree)
import SynTreeSpec (validBoundsSynTree)
import Trees.Print (display)
import TestHelpers (deleteBrackets, deleteSpaces)
import Control.OutputCapable.Blocks (LangM)
import Data.Maybe (isJust,isNothing)
import Control.Monad.Identity (Identity(runIdentity))
import Control.OutputCapable.Blocks.Generic (evalLangM)
import Tasks.LegalProposition.Helpers (formulaAmount)

validBoundsLegalProposition :: Gen LegalPropositionConfig
validBoundsLegalProposition = do
    formulas <- choose (1, 15)
    syntaxTreeConfig@SynTreeConfig {..}  <- validBoundsSynTree
      `suchThat` \cfg -> formulaAmount cfg >= formulas
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

timeout :: Int
timeout = 30000000 -- 30 seconds

spec :: Spec
spec = do
    describe "config" $ do
      it "default config should pass config check" $
        isJust $ runIdentity $ evalLangM (checkLegalPropositionConfig defaultLegalPropositionConfig :: LangM Maybe)
      it "validBoundsLegalProposition should generate a valid config" $
        forAll validBoundsLegalProposition $ \legalPropConfig ->
          isJust $ runIdentity $ evalLangM (checkLegalPropositionConfig legalPropConfig :: LangM Maybe)
    describe "illegalDisplay" $ do
        it "at least creates actual formula symbols" $
            within timeout $ forAll validBoundsSynTree $ \synTreeConfig ->
                forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (deleteSpaces <$> illegalDisplay synTree) $
                      all (\c -> c `elem` "()∧∨¬<=>" || isLetter c)
        it "the string after illegalDisplay cannot be parsed" $
            within timeout $ forAll validBoundsSynTree $ \synTreeConfig ->
                forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (illegalDisplay synTree) $ \str -> isLeft (formulaParse str)
    describe "bracket display" $ do
        it "the String after bracketDisplay just add a bracket " $
            within timeout $ forAll validBoundsSynTree $ \synTreeConfig ->
                forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (bracketDisplay synTree) $ \str -> length str == length (display synTree) + 2
        it "the String can be parsed by formulaParse" $
            within timeout $ forAll validBoundsSynTree $ \synTreeConfig ->
                forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (bracketDisplay synTree) $ \str -> formulaParse str == Right synTree
        it "the String remove all brackets should same with display remove all brackets" $
            within timeout $ forAll validBoundsSynTree $ \synTreeConfig ->
                forAll
                  (genSynTree synTreeConfig) $ \synTree ->
                      forAll (bracketDisplay synTree) $ \str -> deleteBrackets str == deleteBrackets (display synTree)
    describe "generateLegalPropositionInst" $ do
        it "the generateLegalPropositionInst should generate expected illegal number" $
            within timeout $ forAll validBoundsLegalProposition $ \config ->
                forAll (generateLegalPropositionInst config) $ \LegalPropositionInst{..} ->
                  let serialsOfWrong = map fst $ filter (\(_,(_,mt)) -> isNothing mt) (zip [1..] pseudoFormulas) in
                    all (\x -> isLeft (formulaParse (fst (pseudoFormulas !! (x - 1))))) serialsOfWrong
        it "the generateLegalPropositionInst should generate expected legal number" $
            within timeout $ forAll validBoundsLegalProposition $ \config@LegalPropositionConfig{..} ->
                forAll (generateLegalPropositionInst config) $ \LegalPropositionInst{..} ->
                  let serialsOfWrong = map fst $ filter (\(_,(_,mt)) -> isNothing mt) (zip [1..] pseudoFormulas) in
                    all
                    (\x -> isRight (formulaParse (fst (pseudoFormulas !! (x - 1)))))
                    ([1 .. fromIntegral formulas] \\ serialsOfWrong)
