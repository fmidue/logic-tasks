{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module DecideSpec where

import Test.Hspec
import Test.QuickCheck (forAll, Gen, choose, suchThat, elements)
import Control.Monad.Output (LangM)
import Config (dDecideConf, DecideConfig (..), DecideInst (..))
import LogicTasks.Semantics.Decide (verifyQuiz, genDecideInst, verifyStatic)
import Data.Maybe (isJust)
import Control.Monad.Identity (Identity(runIdentity))
import Control.Monad.Output.Generic (evalLangM)
import SynTreeSpec (validBoundsSynTree)
import Formula.Types (Table(getEntries), getTable)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Data.Maybe (fromMaybe)
import Util (withRatio)

validBoundsDecide :: Gen DecideConfig
validBoundsDecide = do
  -- too large tables lead to too long test runs and are probably not suitable for actual tasks
  syntaxTreeConfig <- validBoundsSynTree `suchThat` \SynTreeConfig{..} ->
    maxNodes < 30 &&
    minAmountOfUniqueAtoms == fromIntegral (length availableAtoms)
  percentageOfChanged <- choose (1, 100)
  percentTrueEntriesLow' <- choose (1, 90)
  percentTrueEntriesHigh' <- choose (percentTrueEntriesLow', 99) `suchThat` (/= percentTrueEntriesLow')
  percentTrueEntries <- elements [Just (percentTrueEntriesLow', percentTrueEntriesHigh'), Nothing]

  pure $ DecideConfig {
      syntaxTreeConfig
    , percentageOfChanged
    , percentTrueEntries
    , printSolution = False
    , extraText = Nothing
    }

spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      isJust $ runIdentity $ evalLangM (verifyQuiz dDecideConf :: LangM Maybe)
    it "validBoundsDecide should generate a valid config" $
      forAll validBoundsDecide $ \decideConfig ->
        isJust $ runIdentity $ evalLangM (verifyQuiz decideConfig :: LangM Maybe)
  describe "genDecideInst" $ do
    it "should generate an instance with the right amount of changed entries" $
      forAll validBoundsDecide $ \decideConfig@DecideConfig{..} -> do
        forAll (genDecideInst decideConfig) $ \DecideInst{..} ->
          let tableLen = length (getEntries (getTable tree))
              mistakeCount = max (tableLen * percentageOfChanged `div` 100) 1 in
          length changed == mistakeCount
    it "the generated instance should pass verifyStatic" $
      forAll validBoundsDecide $ \decideConfig -> do
        forAll (genDecideInst decideConfig) $ \decideInst ->
          isJust $ runIdentity $ evalLangM (verifyStatic decideInst :: LangM Maybe)
    it "should respect percentTrueEntries" $
      forAll validBoundsDecide $ \decideConfig@DecideConfig{..} -> do
        forAll (genDecideInst decideConfig) $ \DecideInst{..} ->
          withRatio (fromMaybe (0, 100) percentTrueEntries) tree
