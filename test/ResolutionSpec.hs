{-# LANGUAGE NamedFieldPuns #-}
module ResolutionSpec where

import Test.Hspec
import Formula.Resolution (applySteps)
import Data.Maybe (isJust, fromJust, isNothing)
import Formula.Types (Clause(Clause), Literal (..))
import qualified Data.Set
import Config (ResolutionConfig (..), BaseConfig (..), dResConf, ResolutionInst(solution))
import Test.QuickCheck (Gen, choose, suchThat, forAll)
import LogicTasks.Semantics.Resolve (verifyQuiz, genResInst, completeGrade', partialGrade')
import Control.OutputCapable.Blocks (LangM)
import Control.Monad.Identity (Identity(runIdentity))
import Control.OutputCapable.Blocks.Generic (evalLangM)
import FillSpec (validBoundsBase)

justA :: Clause
justA = Clause (Data.Set.fromList [Positive 'A'])

notAnotB :: Clause
notAnotB = Clause (Data.Set.fromList [Negative 'A', Negative 'B'])

notAjustB :: Clause
notAjustB = Clause (Data.Set.fromList [Negative 'A', Positive 'B'])

notB :: Clause
notB = Clause (Data.Set.fromList [Negative 'B'])

justB :: Clause
justB = Clause (Data.Set.fromList [Positive 'B'])

emptyClause :: Clause
emptyClause = Clause Data.Set.empty

validBoundsResolution :: Gen ResolutionConfig
validBoundsResolution = do
  baseConf <- validBoundsBase
  minSteps <- choose (1,10) `suchThat` \ms ->
    (maxClauseLength baseConf > 1 || ms == 1) && ms <= 2 * length (usedAtoms baseConf)
  pure $ ResolutionConfig {
    baseConf
  , minSteps
  , printFeedbackImmediately = False
  , useSetNotation = True
  , printSolution = False
  , extraText = Nothing
  , offerUnicodeInput = False
  }


spec :: Spec
spec = do
  describe "applySteps" $ do
    it "should return a Just value if there are no clauses" $
      isJust $ applySteps [] []
    it "should return the original list of clauses if there are no steps to apply" $ do
      let clauses = [Clause (Data.Set.fromList [Positive 'A'])]
      fromJust (applySteps clauses []) == clauses
    it "should return the correct list of clauses if the steps are able to be applied" $ do
      let clauses = [justA, notAnotB, notAjustB]
      let steps = [(justA, notAnotB, notB)
                 , (justA, notAjustB, justB)
                 , (notB, justB, emptyClause)]
      let result = fromJust (applySteps clauses steps)
      notB `elem` result && justB `elem` result && emptyClause `elem` result
    it "should return Nothing if some step is invalid" $ do
      let clauses = [justA, notAnotB, notAjustB]
      let steps = [(justA, notAnotB, justB)]
      isNothing $ applySteps clauses steps
    it "should return Nothing if some steps use unavailable clauses" $ do
      let clauses = [justB, notAnotB]
      let steps = [(justB, notB, emptyClause)]
      isNothing $ applySteps clauses steps
  describe "config" $ do
    it "default config should pass config check" $
      isJust $ runIdentity $ evalLangM (verifyQuiz dResConf :: LangM Maybe)
    it "validBoundsResolution should generate a valid config" $
      forAll validBoundsResolution $ \resConfig ->
        isJust $ runIdentity $ evalLangM (verifyQuiz resConfig :: LangM Maybe)
  describe "genResInst" $ do
    it "should required at least minSteps amount of steps" $
      forAll validBoundsResolution $ \resConfig ->
        forAll (genResInst resConfig) $ \resInst ->
          minSteps resConfig <= length (solution resInst)
    it "should generate the correct solution" $
      forAll validBoundsResolution $ \resConfig ->
        forAll (genResInst resConfig) $ \resInst ->
          isJust (runIdentity $ evalLangM (partialGrade' resInst (solution resInst) :: LangM Maybe)) &&
          isJust (runIdentity $ evalLangM (completeGrade' resInst (solution resInst) :: LangM Maybe))

