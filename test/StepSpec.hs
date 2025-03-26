{-# language DuplicateRecordFields #-}
{-# language NamedFieldPuns #-}

module StepSpec where

import Test.Hspec (Spec, describe, it)
import Control.OutputCapable.Blocks (LangM)
import TestHelpers (doesNotRefuse)
import Test.QuickCheck (Gen, chooseAny, forAll)
import Config (StepConfig(..), StepInst(solution), dStepConf, StepAnswer (StepAnswer))
import LogicTasks.Semantics.Step (verifyQuiz, genStepInst, description, verifyStatic, partialGrade', completeGrade')

import FillSpec (validBoundsBaseConfig)



validBoundsStepConfig :: Gen StepConfig
validBoundsStepConfig = do
  baseConf<- validBoundsBaseConfig
  printSolution <- chooseAny
  offerUnicodeInput <- chooseAny
  useSetNotation <- chooseAny
  pure $ StepConfig {
    baseConf,
    useSetNotation,
    printSolution,
    extraText = Nothing,
    offerUnicodeInput
    }


spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (verifyQuiz dStepConf :: LangM Maybe)
    it "validBoundsStepConfig should generate a valid config" $
      forAll validBoundsStepConfig $ \config ->
        doesNotRefuse (verifyQuiz config :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll validBoundsStepConfig $ \config ->
        forAll (genStepInst config) $ \inst ->
          doesNotRefuse (description True inst :: LangM Maybe)
  describe "generateStepInst" $ do
    it "should pass verifyStatic" $
      forAll validBoundsStepConfig $ \config ->
        forAll (genStepInst config) $ \inst ->
          doesNotRefuse
            (verifyStatic inst :: LangM Maybe)
    it "possible solution passes partialGrade" $
      forAll validBoundsStepConfig $ \config ->
        forAll (genStepInst config) $ \inst ->
          doesNotRefuse (partialGrade' inst $ StepAnswer $ Just $ solution inst :: LangM Maybe)
    it "possible solution passes completeGrade" $
      forAll validBoundsStepConfig $ \config ->
        forAll (genStepInst config) $ \inst ->
          doesNotRefuse (completeGrade' inst $ StepAnswer $ Just $ solution inst :: LangM Maybe)
