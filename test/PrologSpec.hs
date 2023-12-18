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
    it "should work" $
      forAll (genPrologInst dPrologConf) $ \PrologInst {..} ->
        isHornClause literals1 && isHornClause literals2
