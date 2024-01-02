{-# LANGUAGE RecordWildCards #-}
module PrologSpec where
import Test.Hspec
import LogicTasks.Semantics.Prolog (genPrologInst)
import Config (dPrologConf, PrologInst (..))
import Formula.Helpers (isHornClause)
import Test.QuickCheck

spec :: Spec
spec = do
  describe "genPrologInst" $
    it "should only generate PrologInst with horn clauses by default" $
      forAll (genPrologInst dPrologConf) $ \PrologInst {..} ->
        isHornClause literals1 && isHornClause literals2
