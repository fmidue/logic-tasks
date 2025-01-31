{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module DecideSpec where

-- jscpd:ignore-start
import Test.Hspec
import Test.QuickCheck (forAll, Gen, choose, suchThat, elements)
import Control.OutputCapable.Blocks (LangM)
import Config (dDecideConf, DecideConfig (..), DecideInst (..), FormulaConfig(..))
import LogicTasks.Semantics.Decide (verifyQuiz, genDecideInst, verifyStatic)
import Data.Maybe (isJust, fromMaybe)
import Control.Monad.Identity (Identity(runIdentity))
import Control.OutputCapable.Blocks.Generic (evalLangM)
import SynTreeSpec (validBoundsSynTree)
import Formula.Types (Table(getEntries), getTable)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Util (withRatio)
import FillSpec (validBoundsCnf)
import LogicTasks.Util (formulaDependsOnAllAtoms)
-- jscpd:ignore-end

validBoundsDecide :: Gen DecideConfig
validBoundsDecide = do
  -- formulaType <- elements ["Cnf", "Dnf", "Arbitrary"]
  let formulaType = "Arbitrary"
  formulaConfig <- case formulaType of
    "Cnf" -> FormulaCnf <$> validBoundsCnf
    "Dnf" -> FormulaDnf <$> validBoundsCnf
    _ -> FormulaArbitrary <$> validBoundsSynTree `suchThat` \SynTreeConfig{..} ->
            maxNodes < 30 &&
            minAmountOfUniqueAtoms == fromIntegral (length availableAtoms)

  percentageOfChanged <- choose (1, 100)
  percentTrueEntriesLow' <- choose (1, 90)
  percentTrueEntriesHigh' <- choose (percentTrueEntriesLow', 99) `suchThat` (/= percentTrueEntriesLow')
  percentTrueEntries <- elements [Just (percentTrueEntriesLow', percentTrueEntriesHigh'), Nothing]

  pure $ DecideConfig {
      formulaConfig
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
          let tableLen = length (getEntries (getTable formula))
              mistakeCount = max (tableLen * percentageOfChanged `div` 100) 1 in
          length changed == mistakeCount
    it "generated formula should depend on all atomics" $
      forAll validBoundsDecide $ \decideConfig@DecideConfig{..} -> do
        forAll (genDecideInst decideConfig) $ \DecideInst{..} ->
          formulaDependsOnAllAtoms formula
    it "the generated instance should pass verifyStatic" $
      forAll validBoundsDecide $ \decideConfig -> do
        forAll (genDecideInst decideConfig) $ \decideInst ->
          isJust $ runIdentity $ evalLangM (verifyStatic decideInst :: LangM Maybe)
    it "should respect percentTrueEntries" $
      forAll validBoundsDecide $ \decideConfig@DecideConfig{..} -> do
        forAll (genDecideInst decideConfig) $ \DecideInst{..} ->
          withRatio (fromMaybe (0, 100) percentTrueEntries) formula

