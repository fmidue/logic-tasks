{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module DecideSpec where

import Test.Hspec
import Test.QuickCheck (forAll, Gen, choose)
import Control.Monad.Output (LangM)
import Config (dDecideConf, DecideConfig (..), DecideInst (..))
import LogicTasks.Semantics.Decide (verifyQuiz, genDecideInst)
import Data.Maybe (isJust)
import Control.Monad.Identity (Identity(runIdentity))
import Control.Monad.Output.Generic (evalLangM)
import SynTreeSpec (validBoundsSynTree)
import Formula.Types (Table(getEntries), getTable)

validBoundsDecide :: Gen DecideConfig
validBoundsDecide = do
  syntaxTreeConfig <- validBoundsSynTree
  percentageOfChanged <- choose (1, 100)

  pure $ DecideConfig {
      syntaxTreeConfig
    , percentageOfChanged
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
    xit "should generate an instance with the right amount of changed entries" $
      forAll validBoundsDecide $ \decideConfig@DecideConfig{..} -> do
        forAll (genDecideInst decideConfig) $ \DecideInst{..} ->
          let tableLen = length (getEntries (getTable tree))
              mistakeCount = max (tableLen * percentageOfChanged `div` 100) 1 in
          length changed == mistakeCount

