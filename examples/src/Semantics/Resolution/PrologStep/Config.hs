module Semantics.Resolution.PrologStep.Config where

import Test.Hspec

import Config (
  PrologConfig(..)
  )
import Formula.Types (positivePLit, queryClause, procedureClause)
import Util.VerifyConfig (noChecker)

small :: PrologConfig
small = PrologConfig
    { minClauseLength = 1
    , maxClauseLength = 3
    , usedPredicates = [positivePLit "f" ["a"], positivePLit "f" ["b"], positivePLit "g" ["a"], positivePLit "g" ["b"]]
    , extraText = Nothing
    , printSolution = True
    , firstClauseShape = queryClause
    , secondClauseShape = procedureClause
    , useSetNotation = False
    }

medium :: PrologConfig
medium = PrologConfig
    { minClauseLength = 2
    , maxClauseLength = 4
    , usedPredicates = [positivePLit "f" ["a","c"], positivePLit "f" ["b","a"], positivePLit "g" ["a"], positivePLit "g" ["c"]]
    , extraText = Nothing
    , printSolution = True
    , firstClauseShape = queryClause
    , secondClauseShape = procedureClause
    , useSetNotation = False
    }

spec :: Spec
spec = do
  describe "small" $ noChecker small
  describe "medium" $ noChecker medium
