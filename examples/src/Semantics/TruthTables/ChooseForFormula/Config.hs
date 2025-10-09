module Semantics.TruthTables.ChooseForFormula.Config where

import LogicTasks.Config (
  BaseConfig(..),
  PickConfig(..),
  NormalFormConfig(..),
  FormulaConfig(..)
  )

import Test.Hspec
import Util.VerifyConfig

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
                  , percentPosLiterals = (0,100)
                  })
  , amountOfOptions = 3
  , percentTrueEntries = Nothing
  , extraText = Nothing
  , printSolution = True
  }

-- 2024: Weight 0.3
task11 :: PickConfig
task11 = PickConfig
  { formulaConfig =
      FormulaCnf (NormalFormConfig
                   { baseConf = BaseConfig
                     { minClauseLength = 3
                     , maxClauseLength = 3
                     , usedAtoms = "ABCDE"
                     }
                   , minClauseAmount = 4
                   , maxClauseAmount = 4
                   , percentPosLiterals = (0,100)
                   })
  , amountOfOptions = 4
  , percentTrueEntries = Just (30,70)
  , printSolution = True
  , extraText = Nothing
  }

spec :: Spec
spec = do
  describe "task08" $ verifyFormulaConfig (formulaConfig task08)
  describe "task11" $ verifyFormulaConfig (formulaConfig task11)
