module ResolutionSpec where

import Test.Hspec
import Formula.Resolution (applySteps)
import Data.Maybe (isJust, fromJust, isNothing)
import Formula.Types (Clause(Clause), Literal (Literal,Not))
import qualified Data.Set

justA :: Clause
justA = Clause (Data.Set.fromList [Literal 'A'])

notAnotB :: Clause
notAnotB = Clause (Data.Set.fromList [Not 'A', Not 'B'])

notAjustB :: Clause
notAjustB = Clause (Data.Set.fromList [Not 'A', Literal 'B'])

notB :: Clause
notB = Clause (Data.Set.fromList [Not 'B'])

justB :: Clause
justB = Clause (Data.Set.fromList [Literal 'B'])

emptyClause :: Clause
emptyClause = Clause Data.Set.empty

spec :: Spec
spec = do
  describe "applySteps" $ do
    it "should return a Just value if there are no clauses" $
      isJust $ applySteps [] []
    it "should return the original list of clauses if there are no steps to apply" $ do
      let clauses = [Clause (Data.Set.fromList [Literal 'A'])]
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
