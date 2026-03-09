module Semantics.TruthTables.ChooseForFormula.Config where

import LogicTasks.Config (
  BaseConfig(..),
  PickConfig(..),
  NormalFormConfig(..),
  FormulaConfig(..)
  )

import Test.Hspec
import Util.VerifyConfig
import Formula.Util(PercentRangeMode(TrueEntries))

-- 2023: Weight 0.33
task08 :: PickConfig
task08 = PickConfig
  { formulaConfig =
      FormulaCnf (NormalFormConfig
                  { baseConf = BaseConfig
                    { minClauseLength = 3
                    , maxClauseLength = 3
                    , usedAtoms = "ABCD"
                    }
                  , minClauseAmount = 2
                  , maxClauseAmount = 2
                  })
  , amountOfOptions = 3
  , percentRangeMode = TrueEntries (0, 100)
  , extraText = Nothing
  , printSolution = True
  }

-- 2025: Weight 0.3
task12 :: PickConfig
task12 = PickConfig
  { formulaConfig =
      FormulaCnf (NormalFormConfig
                   { baseConf = BaseConfig
                     { minClauseLength = 3
                     , maxClauseLength = 3
                     , usedAtoms = "ABCDE"
                     }
                   , minClauseAmount = 4
                   , maxClauseAmount = 4
                   })
  , amountOfOptions = 4
  , percentRangeMode = TrueEntries (30, 70)
  , printSolution = True
  , extraText = Nothing
  }

spec :: Spec
spec = do
  describe "task08" $ verifyFormulaConfig (formulaConfig task08)
  describe "task12" $ verifyFormulaConfig (formulaConfig task12)
