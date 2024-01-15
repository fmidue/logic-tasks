{-# LANGUAGE RecordWildCards #-}
module PrologSpec where
import Test.Hspec
import LogicTasks.Semantics.Prolog (genPrologInst)
import Config (dPrologConf, PrologInst (..))
import Formula.Helpers (hasTheClauseShape)
import Test.QuickCheck
import LogicTasks.Config (PrologConfig (firstClauseShape, secondClauseShape))

spec :: Spec
spec = do
  describe "genPrologInst" $
    it "should only generate PrologInst with horn clauses by default" $
      forAll (genPrologInst dPrologConf) $ \PrologInst {..} ->
        hasTheClauseShape (firstClauseShape dPrologConf) literals1
          && hasTheClauseShape (secondClauseShape dPrologConf) literals2
