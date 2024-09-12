{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module FillSpec where

-- jscpd:ignore-start
import Test.Hspec
import Test.QuickCheck (forAll, Gen, choose, elements, suchThat, sublistOf)
import Control.OutputCapable.Blocks (LangM)
import Config (dFillConf, FillConfig (..), FillInst (..), FormulaConfig(..), BaseConfig(..), dBaseConf, CnfConfig(..), dCnfConf)
import LogicTasks.Semantics.Fill (verifyQuiz, genFillInst, verifyStatic, partialGrade, completeGrade)
import Data.Maybe (isJust, fromMaybe)
import Control.Monad.Identity (Identity(runIdentity))
import Control.OutputCapable.Blocks.Generic (evalLangM)
import SynTreeSpec (validBoundsSynTree)
import Formula.Types (Table(getEntries), getTable, lengthBound, TruthValue (TruthValue))
import Tasks.SynTree.Config (SynTreeConfig(..))
import Util (withRatio, checkBaseConf, checkCnfConf)
-- jscpd:ignore-end

validBoundsBase :: Gen BaseConfig
validBoundsBase = do
  minClauseLength <- choose (1, 5)
  maxClauseLength <- choose (2, 10) `suchThat` \x -> minClauseLength <= x
  usedLiterals <- sublistOf ['A' .. 'Z'] `suchThat` \xs -> length xs >= maxClauseLength
  pure $ BaseConfig {
    minClauseLength
  , maxClauseLength
  , usedLiterals
  }

validBoundsCnf :: Gen CnfConfig
validBoundsCnf = do
  minClauseAmount <- choose (1, 5)
  maxClauseAmount <- choose (2, 10) `suchThat` \x -> minClauseAmount <= x
  baseConf <- validBoundsBase `suchThat` \bc ->
    minClauseAmount * minClauseLength bc >= length (usedLiterals bc) &&
    minClauseAmount <= 2 ^ length (usedLiterals bc) &&
    minClauseAmount <= lengthBound (length (usedLiterals bc)) (maxClauseLength bc)
  pure $ CnfConfig {
    baseConf
  , minClauseAmount
  , maxClauseAmount
  }

validBoundsFill :: Gen FillConfig
validBoundsFill = do
  -- formulaType <- elements ["Cnf", "Dnf", "Arbitrary"]
  let formulaType = "Arbitrary"
  formulaConfig <- case formulaType of
    "Cnf" -> FormulaCnf <$> validBoundsCnf
    "Dnf" -> FormulaDnf <$> validBoundsCnf
    _ -> FormulaArbitrary <$> validBoundsSynTree `suchThat` \SynTreeConfig{..} ->
            maxNodes < 30 &&
            minAmountOfUniqueAtoms == fromIntegral (length availableAtoms)

  percentageOfGaps <- choose (1, 100)
  percentTrueEntriesLow' <- choose (0, 90)
  percentTrueEntriesHigh' <- choose (percentTrueEntriesLow', 100) `suchThat` (/= percentTrueEntriesLow')
  percentTrueEntries <- elements [Just (percentTrueEntriesLow', percentTrueEntriesHigh'), Nothing]

  pure $ FillConfig {
      formulaConfig
    , percentageOfGaps
    , percentTrueEntries
    , printSolution = False
    , extraText = Nothing
    }

spec :: Spec
spec = do
  describe "BaseConfig" $ do
    it "default base config should pass config check" $
      isJust $ runIdentity $ evalLangM (checkBaseConf dBaseConf :: LangM Maybe)
    it "validBoundsBase should generate a valid config" $
      forAll validBoundsBase $ \baseConfig ->
        isJust $ runIdentity $ evalLangM (checkBaseConf baseConfig :: LangM Maybe)
  describe "CnfConfig" $ do
    it "default cnf config should pass config check" $
      isJust $ runIdentity $ evalLangM (checkCnfConf dCnfConf :: LangM Maybe)
    it "validBoundsCnf should generate a valid config" $
      forAll validBoundsCnf $ \cnfConfig ->
        isJust $ runIdentity $ evalLangM (checkCnfConf cnfConfig :: LangM Maybe)
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
          let tableLen = length (getEntries (getTable formula))
              gapCount = max (tableLen * percentageOfGaps `div` 100) 1 in
          length missing == gapCount
    it "should respect percentTrueEntries" $
      forAll validBoundsFill $ \fillConfig@FillConfig{..} ->
        forAll (genFillInst fillConfig) $ \FillInst{..} ->
          withRatio (fromMaybe (0, 100) percentTrueEntries) formula
    it "the generated instance should pass verifyStatic" $
      forAll validBoundsFill $ \fillConfig -> do
        forAll (genFillInst fillConfig) $ \fillInst ->
          isJust $ runIdentity $ evalLangM (verifyStatic fillInst :: LangM Maybe)
    it "the generated solution should pass grading" $
      forAll validBoundsFill $ \fillConfig -> do
        forAll (genFillInst fillConfig) $ \fillInst ->
          isJust (runIdentity (evalLangM (partialGrade fillInst (map TruthValue (missingValues fillInst))  :: LangM Maybe))) &&
          isJust (runIdentity (evalLangM (completeGrade fillInst (map TruthValue (missingValues fillInst))  :: LangM Maybe)))

