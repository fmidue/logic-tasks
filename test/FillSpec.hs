{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module FillSpec where

import Test.Hspec
import Test.QuickCheck (forAll, Gen, choose, elements)
import Control.Monad.Output (LangM)
import Config (dFillConf, FillConfig (..), FillInst (..))
import LogicTasks.Semantics.Fill (verifyQuiz, genFillInst)
import Data.Maybe (isJust)
import Control.Monad.Identity (Identity(runIdentity))
import Control.Monad.Output.Generic (evalLangM)
import SynTreeSpec (validBoundsSynTree)
import Formula.Types (Table(getEntries), getTable)

validBoundsFill :: Gen FillConfig
validBoundsFill = do
  syntaxTreeConfig <- validBoundsSynTree
  percentageOfGaps <- choose (1, 100)
  percentTrueEntriesLow' <- choose (0, 100)
  percentTrueEntriesHigh' <- choose (percentTrueEntriesLow', 100)
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
    xit "should generate an instance with the right amount of gaps" $
      forAll validBoundsFill $ \fillConfig@FillConfig{..} -> do
        forAll (genFillInst fillConfig) $ \FillInst{..} ->
          let tableLen = length (getEntries (getTable tree))
              gapCount = max (tableLen * percentageOfGaps `div` 100) 1 in
          length missing == gapCount
    it "should respect percentTrueEntries" $
      pendingWith "Needs to be reimplemented"

