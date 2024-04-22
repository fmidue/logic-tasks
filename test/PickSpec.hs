{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module PickSpec where
import Test.Hspec (Spec, describe, it)
import Control.Monad.Output (LangM)
import Config (dPickConf, PickConfig (..), PickInst (..))
import LogicTasks.Semantics.Pick (verifyQuiz, genPickInst, verifyStatic)
import Data.Maybe (isJust, fromMaybe)
import Control.Monad.Identity (Identity(runIdentity))
import Control.Monad.Output.Generic (evalLangM)
import Test.QuickCheck (Gen, choose, forAll, suchThat, elements)
import SynTreeSpec (validBoundsSynTree)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Data.List (nubBy)
import Formula.Util (isSemanticEqual)
import Trees.Helpers (collectLeaves)
import Data.List.Extra (nubOrd, nubSort)
import Util (withRatio)

validBoundsPick :: Gen PickConfig
validBoundsPick = do
  amountOfOptions <- choose (2, 5)
  syntaxTreeConfig <- validBoundsSynTree `suchThat` \SynTreeConfig{..} ->
    amountOfOptions <= 4*2^ length availableAtoms &&
    minAmountOfUniqueAtoms == fromIntegral (length availableAtoms)

  percentTrueEntriesLow' <- choose (1, 90)
  percentTrueEntriesHigh' <- choose (percentTrueEntriesLow', 99) `suchThat` (/= percentTrueEntriesLow')
  percentTrueEntries <- elements [Just (percentTrueEntriesLow', percentTrueEntriesHigh'), Nothing]

  pure $ PickConfig {
      syntaxTreeConfig
    , amountOfOptions
    , percentTrueEntries
    , printSolution = False
    , extraText = Nothing
    }

spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      isJust $ runIdentity $ evalLangM (verifyQuiz dPickConf :: LangM Maybe)
    it "validBoundsPick should generate a valid config" $
      forAll validBoundsPick $ \pickConfig ->
        isJust $ runIdentity $ evalLangM (verifyQuiz pickConfig :: LangM Maybe)
  describe "genPickInst" $ do
    it "generated formulas should not be semantically equivalent" $
      forAll validBoundsPick $ \pickConfig@PickConfig{..} ->
        forAll (genPickInst pickConfig) $ \PickInst{..} ->
          length (nubBy isSemanticEqual trees) == amountOfOptions
    it "generated formulas should only consist of the same atomics" $
      forAll validBoundsPick $ \pickConfig ->
        forAll (genPickInst pickConfig) $ \PickInst{..} ->
          length (nubOrd (map (nubSort . collectLeaves) trees)) == 1
    it "the generated instance should pass verifyStatic" $
      forAll validBoundsPick $ \pickConfig -> do
        forAll (genPickInst pickConfig) $ \pickInst ->
          isJust $ runIdentity $ evalLangM (verifyStatic pickInst :: LangM Maybe)
    it "should respect percentTrueEntries" $
      forAll validBoundsPick $ \pickConfig@PickConfig{..} ->
        forAll (genPickInst pickConfig) $ \PickInst{..} ->
          all (withRatio (fromMaybe (0, 100) percentTrueEntries)) trees
