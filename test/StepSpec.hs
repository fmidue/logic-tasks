{-# LANGUAGE NamedFieldPuns #-}
module StepSpec where

import Test.Hspec (Spec, describe, it)
import Control.OutputCapable.Blocks (LangM)
import TestHelpers (doesNotRefuse)
import Test.QuickCheck (forAll, Gen)
import Config (StepInst(solution), dStepConf, StepAnswer (StepAnswer), StepConfig (..))
import LogicTasks.Semantics.Step (verifyQuiz, genStepInst, description, verifyStatic, partialGrade', completeGrade')
import FillSpec (validBoundsBaseConfig)


validBoundsStepConfig :: Gen StepConfig
validBoundsStepConfig = do
  baseConf <- validBoundsBaseConfig
  return $ StepConfig
    { baseConf
    , useSetNotation = False
    , printSolution = False
    , extraText = Nothing
    , offerUnicodeInput = False
    }

spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (verifyQuiz dStepConf :: LangM Maybe)
    it "validBoundsStepConfig should pass config check" $
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
