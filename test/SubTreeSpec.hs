{-# LANGUAGE RecordWildCards, NamedFieldPuns, TypeApplications #-}
module SubTreeSpec (spec) where

import Capabilities.Cache.IO ()
import Capabilities.LatexSvg.IO ()
import Test.Hspec (describe, it, xit, Spec)
import Test.QuickCheck (Gen, chooseInteger, forAll, elements, suchThat, ioProperty)
import Text.Parsec (parse)
import Data.Either.Extra (fromRight')
import Data.List.Extra (isInfixOf )
import Data.Set (size, toList)
import qualified Data.Set (map)

import Tasks.SubTree.Config (SubTreeConfig(..), SubTreeInst(..), checkSubTreeConfig, defaultSubTreeConfig)
import Tasks.SubTree.Quiz (generateSubTreeInst)
import Trees.Helpers (allNotLeafSubTrees, maxLeavesForNodes)
import Tasks.SynTree.Config (SynTreeConfig(..),)
import TestHelpers (deleteSpaces, doesNotRefuse, doesNotRefuseIO)
import Trees.Print (display)
import Trees.Parsing ()
import Trees.Types (SynTree, BinOp, PropFormula, FormulaAnswer (FormulaAnswer))
import SynTreeSpec (validBoundsSynTreeConfig)
import Formula.Parsing (Parse(parser))
import Control.OutputCapable.Blocks (LangM)
import LogicTasks.Syntax.SubTreeSet (description, verifyInst, partialGrade', completeGrade')
import System.IO.Temp (withSystemTempDirectory)
import Test.QuickCheck.Property (within)

validBoundsSubTreeConfig :: Gen SubTreeConfig
validBoundsSubTreeConfig = do
    allowSameSubTree <- elements [True,False]
    syntaxTreeConfig@SynTreeConfig {..} <- validBoundsSynTreeConfig `suchThat`
      (\synTreeConf -> 4 <= minNodes synTreeConf && minAmountOfUniqueAtoms synTreeConf > maxNodes synTreeConf `div` 4)
    subTreeAmount <- chooseInteger (2, minNodes - maxLeavesForNodes minNodes)
    return $ SubTreeConfig
      {
        syntaxTreeConfig
      , allowSameSubTree
      , subTreeAmount
      , extraText = Nothing
      , printSolution = False
      , offerUnicodeInput = False
      }

computeSolution :: Integer -> [SynTree BinOp Char] -> [FormulaAnswer]
computeSolution n trees =
  take (fromIntegral n)
  $ map (FormulaAnswer . Just . fromRight' . parse parser "Input" . display) trees

spec :: Spec
spec = do
  describe "config" $ do
      it "default config should pass config check" $
        doesNotRefuse (checkSubTreeConfig defaultSubTreeConfig :: LangM Maybe)
      it "validBoundsSubTreeConfig should generate a valid config" $
        forAll validBoundsSubTreeConfig $ \subTreeConfig ->
          doesNotRefuse (checkSubTreeConfig subTreeConfig :: LangM Maybe)
  describe "description" $ do
      it "should not reject" $
       within (30 * 1000000) $ forAll validBoundsSubTreeConfig $ \config ->
        forAll (generateSubTreeInst config) $ \inst ->
            doesNotRefuse (description False inst :: LangM Maybe)
  describe "generateSubTreeInst" $ do
    it "parse should works well" $
      within (30 * 1000000) $ forAll validBoundsSubTreeConfig $ \subTreeConfig ->
        forAll (generateSubTreeInst subTreeConfig) $ \SubTreeInst{..} ->
          let
            correctTrees = allNotLeafSubTrees tree
          in
            all (\tree -> parse (parser @(SynTree BinOp Char)) "" (display tree) == Right tree) correctTrees
    it "correct formulas are stored" $
      within (30 * 1000000) $ forAll validBoundsSubTreeConfig $ \subTreeConfig ->
        forAll (generateSubTreeInst subTreeConfig) $ \SubTreeInst{..} ->
          let
            correctTrees' = allNotLeafSubTrees tree
          in
            correctTrees == correctTrees'
    it "it should generate not less Syntax Sub tree number it required as excepted" $
      within (30 * 1000000) $ forAll validBoundsSubTreeConfig $ \config@SubTreeConfig {..} ->
        forAll (generateSubTreeInst config) $ \SubTreeInst{..} ->
          fromIntegral (size correctTrees) >= subTreeAmount
    it "all subformulas are the sublist of the formula" $
      within (30 * 1000000) $ forAll validBoundsSubTreeConfig $ \config@SubTreeConfig {..} ->
        forAll (generateSubTreeInst config) $ \SubTreeInst{..} ->
          let
            correctFormulas = Data.Set.map display correctTrees
          in
            all (`isInfixOf` display tree) correctFormulas
    it "Converting correct subformulas Strings into formulas and parsing them again should yield the original" $
      within (30 * 1000000) $ forAll validBoundsSubTreeConfig $ \config ->
          forAll (generateSubTreeInst config) $ \SubTreeInst{..} ->
            let
              correctFormulas = Data.Set.map display correctTrees
              propFormulas = Data.Set.map
                (fromRight' . parse (parser @(PropFormula Char)) "")
                correctFormulas
              inputSet = Data.Set.map show propFormulas
            in
              inputSet == correctFormulas
    xit "The above should be true even when deleting spaces in the input" $
      within (30 * 1000000) $ forAll validBoundsSubTreeConfig $ \config ->
        forAll (generateSubTreeInst config) $ \SubTreeInst{..} ->
          let
            correctFormulas = Data.Set.map display correctTrees
            propFormulas = Data.Set.map
              (fromRight' . parse (parser @(PropFormula Char)) "" . deleteSpaces)
              correctFormulas
            inputSet = Data.Set.map show propFormulas
          in
            inputSet == correctFormulas
    it "should pass verifyInst" $
      within (30 * 1000000) $ forAll validBoundsSubTreeConfig $ \subTreeConfig ->
        forAll (generateSubTreeInst subTreeConfig) $ \inst ->
          doesNotRefuse (verifyInst inst :: LangM Maybe)
    it "should pass partialGrade" $
      within (30 * 1000000) $ forAll validBoundsSubTreeConfig $ \subTreeConfig ->
        forAll (generateSubTreeInst subTreeConfig) $ \inst@SubTreeInst{..} ->
          doesNotRefuse (partialGrade' inst (computeSolution inputTreeAmount $ toList correctTrees) :: LangM Maybe)
    it "should pass completeGrade" $
      within (30 * 1000000) $ forAll validBoundsSubTreeConfig $ \subTreeConfig ->
        forAll (generateSubTreeInst subTreeConfig) $ \inst@SubTreeInst{..} -> ioProperty $
            withSystemTempDirectory "logic-tasks" $ \path ->
              doesNotRefuseIO (completeGrade' path inst (computeSolution inputTreeAmount $ toList correctTrees))
