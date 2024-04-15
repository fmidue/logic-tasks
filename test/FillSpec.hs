{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module FillSpec where

import Test.Hspec
import Test.QuickCheck (forAll, Gen, choose, elements, suchThat)
import Control.Monad.Output (LangM)
import Config (dFillConf, FillConfig (..), FillInst (..))
import LogicTasks.Semantics.Fill (verifyQuiz, genFillInst, verifyStatic)
import Data.Maybe (isJust, fromMaybe)
import Control.Monad.Identity (Identity(runIdentity))
import Control.Monad.Output.Generic (evalLangM)
import SynTreeSpec (validBoundsSynTree)
import Formula.Types (Table(getEntries), getTable)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Util (withRatio)

validBoundsFill :: Gen FillConfig
validBoundsFill = do
  -- too large tables lead to too long test runs and are probably not suitable for actual tasks
  syntaxTreeConfig <- validBoundsSynTree `suchThat` \SynTreeConfig{..} -> maxNodes < 30
  percentageOfGaps <- choose (1, 100)
  percentTrueEntriesLow' <- choose (0, 90)
  percentTrueEntriesHigh' <- choose (percentTrueEntriesLow', 100) `suchThat` (/= percentTrueEntriesLow')
  percentTrueEntries <- elements [Just (percentTrueEntriesLow', percentTrueEntriesHigh'), Nothing]

  pure $ FillConfig {
      syntaxTreeConfig
    , percentageOfGaps
    , percentTrueEntries
    , printSolution = False
    , extraText = Nothing
    }

spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      isJust $ runIdentity $ evalLangM (verifyQuiz dFillConf :: LangM Maybe)
    it "validBoundsFill should generate a valid config" $
      forAll validBoundsFill $ \fillConfig ->
        isJust $ runIdentity $ evalLangM (verifyQuiz fillConfig :: LangM Maybe)
  describe "genFillInst" $ do
    it "should generate an instance with the right amount of gaps" $
      forAll validBoundsFill $ \fillConfig@FillConfig{..} -> do
        forAll (genFillInst fillConfig) $ \FillInst{..} ->
          let tableLen = length (getEntries (getTable tree))
              gapCount = max (tableLen * percentageOfGaps `div` 100) 1 in
          length missing == gapCount
    it "should respect percentTrueEntries" $
      forAll validBoundsFill $ \fillConfig@FillConfig{..} ->
        forAll (genFillInst fillConfig) $ \FillInst{..} ->
          withRatio (fromMaybe (0, 100) percentTrueEntries) tree
    it "the generated instance should pass verifyStatic" $
      forAll validBoundsFill $ \fillConfig -> do
        forAll (genFillInst fillConfig) $ \fillInst ->
          isJust $ runIdentity $ evalLangM (verifyStatic fillInst :: LangM Maybe)

