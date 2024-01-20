module TableSpec where

import Test.Hspec
import Test.QuickCheck
import LogicTasks.Formula
import LogicTasks.Table
import qualified Data.Set as Set



equivGen :: Gen (Cnf,Cnf)
equivGen = sized equiv
  where
    equiv n = do
        cnf <- resize n arbitrary
        let clauses = Set.fromList $ getClauses cnf
        if Set.null clauses
          then equivGen
          else do
            let lits = Set.unions $ Set.map (Set.fromList . literals) clauses
            clause <- elements (Set.toList clauses)
            let clauseLits = Set.fromList $ literals clause
                availLits = lits Set.\\ clauseLits
            if Set.null availLits
              then equivGen
              else do
                chosenLit <- elements (Set.toList availLits)
                let newClause = mkClause $ Set.toList $ Set.insert chosenLit clauseLits
                let newCnf = mkCnf $ Set.toList $ Set.insert newClause clauses
                pure (cnf,newCnf)



spec :: Spec
spec = tableGenSpec



tableGenSpec :: Spec
tableGenSpec =
  describe "getTable" $ do
    context "When generating different tables" $
      it "should have used different formulas" $
        forAll (applySize (arbitrary :: Gen (Cnf,Cnf))) $ \(x,y) -> getTable x /= getTable y ==> x /= y

    context "When using equivalent formulas" $
      it "should produce the same table twice" $
        forAll (applySize equivGen) $ \(x,y) -> getTable x == getTable y

    context "When looking at each row of the generated table" $
      it "should have truth values equal to the formula being evaluated with the row's allocation" $
        forAll (applySize (arbitrary :: Gen Cnf)) $ \x ->
          map (`evaluate` x) (possibleAllocations (atomics x)) == readEntries (getTable x)





applySize :: Gen a -> Gen a
applySize = resize 8
