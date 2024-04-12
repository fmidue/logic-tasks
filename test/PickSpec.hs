{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module PickSpec where
import Test.Hspec (Spec, describe, it, xit)
import Control.Monad.Output (LangM)
import Config (dPickConf, PickConfig (..), PickInst (..))
import LogicTasks.Semantics.Pick (verifyQuiz, genPickInst)
import Data.Maybe (isJust)
import Control.Monad.Identity (Identity(runIdentity))
import Control.Monad.Output.Generic (evalLangM)
import Test.QuickCheck (Gen, choose, forAll, suchThat)
import SynTreeSpec (validBoundsSynTree)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Data.List (nubBy)
import Formula.Util (isSemanticEqual)
import Trees.Helpers (collectLeaves)
import Data.List.Extra (nubOrd, nubSort)

validBoundsPick :: Gen PickConfig
validBoundsPick = do
  amountOfOptions <- choose (2, 5)
  syntaxTreeConfig <- validBoundsSynTree `suchThat` \SynTreeConfig{..} ->
    amountOfOptions <= 4*2^ length availableAtoms

  pure $ PickConfig {
      syntaxTreeConfig
    , amountOfOptions
    , pickCnf = False
    , printSolution = False
    , extraText = Nothing
    }

spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      isJust $ runIdentity $ evalLangM (verifyQuiz dPickConf :: LangM Maybe)
    it "validBoundsPick should generate a valid config" $
      forAll validBoundsPick $ \pickConfig ->
        isJust $ runIdentity $ evalLangM (verifyQuiz pickConfig :: LangM Maybe)
  describe "genPickInst" $ do
    xit "generated formulas should not be semantically equivalent" $
      forAll validBoundsPick $ \pickConfig@PickConfig{..} ->
        forAll (genPickInst pickConfig) $ \PickInst{..} ->
          length (nubBy isSemanticEqual trees) == amountOfOptions
    xit "generated formulas should only consist of the same atomics" $
      forAll validBoundsPick $ \pickConfig ->
        forAll (genPickInst pickConfig) $ \PickInst{..} ->
          length (nubOrd (map (nubSort . collectLeaves) trees)) == 1
