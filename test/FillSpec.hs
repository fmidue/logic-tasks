{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module FillSpec where

-- jscpd:ignore-start
import Test.Hspec
import Test.QuickCheck (forAll, Gen, chooseInt, suchThat)
import Control.OutputCapable.Blocks (LangM, Rated)
import Config (
  dFillConf,
  FillConfig (..),
  FillInst (..),
  FormulaConfig(..),
  BaseConfig(..),
  dBaseConf,
  NormalFormConfig(..),
  dNormalFormConf
 )
import LogicTasks.Semantics.Fill (verifyQuiz, genFillInst, verifyStatic, partialGrade, completeGrade, description)
import SynTreeSpec (validBoundsSynTreeConfig')
import Formula.Types (Table(getEntries), getTable, lengthBound, TruthValue (TruthValue))
import Formula.Util (withPercentRange, PercentRangeMode(TrueEntries, PosLiterals))
import Tasks.SynTree.Config (SynTreeConfig(..))
import Util (checkBaseConf, checkNormalFormConfig)
import LogicTasks.Util (formulaDependsOnAllAtoms)
import TestHelpers (doesNotRefuse, genSubsetOf)
import Test.QuickCheck.Property (within)
import Test.QuickCheck.Gen (oneof)
-- jscpd:ignore-end

validBoundsBaseConfig :: Gen BaseConfig
validBoundsBaseConfig = do
  minClauseLength <- chooseInt (1, 5)
  maxClauseLength <- chooseInt (max 2 minClauseLength, 10)
  usedAtoms <- genSubsetOf (maxClauseLength, 26) ['A' .. 'Z']
  pure $ BaseConfig {
    minClauseLength
  , maxClauseLength
  , usedAtoms
  }

validBoundsNormalFormConfig :: Gen NormalFormConfig
validBoundsNormalFormConfig = do
  minClauseAmount <- chooseInt (1, 5)
  maxClauseAmount <- chooseInt (max 2 minClauseAmount, 10)
  baseConf <- validBoundsBaseConfig `suchThat` \bc ->
    minClauseAmount * minClauseLength bc >= length (usedAtoms bc) &&
    minClauseAmount <= 2 ^ length (usedAtoms bc) &&
    minClauseAmount <= lengthBound (length (usedAtoms bc)) (maxClauseLength bc)
  pure $ NormalFormConfig {
    baseConf
  , minClauseAmount
  , maxClauseAmount
  }

validPercentRangeModes :: FormulaConfig -> Gen PercentRangeMode
validPercentRangeModes formulaConfig =
  oneof
    [ TrueEntries <$> validBoundsPercentTrueEntries formulaConfig
    , PosLiterals <$> validBoundsPercentPosLiterals formulaConfig
    ]

validBoundsPercentTrueEntries :: FormulaConfig -> Gen (Int, Int)
validBoundsPercentTrueEntries formulaConfig = do
  case formulaConfig of
    FormulaDnf normalFormConfig -> do
      let entries = (2 ^ length (usedAtoms (baseConf normalFormConfig))) :: Int
      validRange entries
    FormulaCnf normalFormConfig -> do
      let entries = (2 ^ length (usedAtoms (baseConf normalFormConfig))) :: Int
      validRange entries
    FormulaArbitrary _ -> pure (0, 100)
  where
    validRange entries = do
      trueEntriesLow <- chooseInt (1,entries - 1)
      trueEntriesHigh <- chooseInt (trueEntriesLow + 1, entries)
      pure (
        floor (fromIntegral (trueEntriesLow * 100) / fromIntegral entries),
        ceiling (fromIntegral (trueEntriesHigh * 100) / fromIntegral entries)
        )

validBoundsPercentPosLiterals :: FormulaConfig -> Gen (Int, Int)
validBoundsPercentPosLiterals formulaConfig = do
  case formulaConfig of
    FormulaDnf normalFormConfig -> do
      let entries = minClauseLength (baseConf normalFormConfig) * minClauseAmount normalFormConfig
      validRange entries
    FormulaCnf normalFormConfig -> do
      let entries = minClauseLength (baseConf normalFormConfig) * minClauseAmount normalFormConfig
      validRange entries
    FormulaArbitrary _ -> pure (0, 100)
  where
    validRange entries = do
      trueEntriesLow <- chooseInt (1,entries - 1)
      trueEntriesHigh <- chooseInt (trueEntriesLow + 1, entries)
      pure (
        floor (fromIntegral (trueEntriesLow * 100) / fromIntegral entries),
        ceiling (fromIntegral (trueEntriesHigh * 100) / fromIntegral entries)
        )

validBoundsFillConfig :: Gen FillConfig
validBoundsFillConfig = do
  -- formulaType <- elements ["Cnf", "Dnf", "Arbitrary"]
  let formulaType = "Arbitrary"
  formulaConfig <- case formulaType of
    "Cnf" -> FormulaCnf <$> validBoundsNormalFormConfig
    "Dnf" -> FormulaDnf <$> validBoundsNormalFormConfig
    _ -> FormulaArbitrary <$> validBoundsSynTreeConfig' False `suchThat` \SynTreeConfig{..} ->
            maxNodes < 30

  percentageOfGaps <- chooseInt (1, 100)
  percentRangeMode <- validPercentRangeModes formulaConfig

  pure $ FillConfig {
      formulaConfig
    , percentageOfGaps
    , percentRangeMode
    , printSolution = False
    , extraText = Nothing
    }

spec :: Spec
spec = do
  describe "BaseConfig" $ do
    it "default base config should pass config check" $
      doesNotRefuse (checkBaseConf dBaseConf :: LangM Maybe)
    it "validBoundsBaseConfig should generate a valid config" $
      forAll validBoundsBaseConfig $ \baseConfig ->
        doesNotRefuse (checkBaseConf baseConfig :: LangM Maybe)
  describe "NormalFormConfig" $ do
    it "default cnf config should pass config check" $
      doesNotRefuse (checkNormalFormConfig dNormalFormConf :: LangM Maybe)
    it "validBoundsNormalFormConfig should generate a valid config" $
      forAll validBoundsNormalFormConfig $ \normalFormConfig ->
        doesNotRefuse (checkNormalFormConfig normalFormConfig :: LangM Maybe)
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (verifyQuiz dFillConf :: LangM Maybe)
    it "validBoundsFillConfig should generate a valid config" $
      forAll validBoundsFillConfig $ \fillConfig ->
        doesNotRefuse (verifyQuiz fillConfig :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll validBoundsFillConfig $ \fillConfig@FillConfig{..} -> do
        within (30 * 1000000) $ forAll (genFillInst fillConfig) $ \inst ->
          doesNotRefuse (description False inst :: LangM Maybe)
  describe "genFillInst" $ do
    it "should generate an instance with the right amount of gaps" $
      forAll validBoundsFillConfig $ \fillConfig@FillConfig{..} -> do
        within (30 * 1000000) $ forAll (genFillInst fillConfig) $ \FillInst{..} ->
          let tableLen = length (getEntries (getTable formula))
              gapCount = max (tableLen * percentageOfGaps `div` 100) 1 in
          length missing == gapCount
    it "generated formula should depend on all atomics" $
     forAll validBoundsFillConfig $ \fillConfig@FillConfig{..} -> do
        within (30 * 1000000) $ forAll (genFillInst fillConfig) $ \FillInst{..} ->
          formulaDependsOnAllAtoms formula
    it "should respect percentRangeMode" $
      forAll validBoundsFillConfig $ \fillConfig@FillConfig{..} ->
        within (30 * 1000000) $ forAll (genFillInst fillConfig) $ \FillInst{..} ->
          withPercentRange percentRangeMode formula
    it "the generated instance should pass verifyStatic" $
      forAll validBoundsFillConfig $ \fillConfig -> do
        within (30 * 1000000) $ forAll (genFillInst fillConfig) $ \fillInst ->
          doesNotRefuse (verifyStatic fillInst :: LangM Maybe)
    it "the generated solution should pass grading" $
      forAll validBoundsFillConfig $ \fillConfig -> do
        within (30 * 1000000) $ forAll (genFillInst fillConfig) $ \fillInst ->
          doesNotRefuse (partialGrade fillInst (map TruthValue (missingValues fillInst))  :: LangM Maybe) &&
          doesNotRefuse (completeGrade fillInst (map TruthValue (missingValues fillInst))  :: Rated Maybe)

