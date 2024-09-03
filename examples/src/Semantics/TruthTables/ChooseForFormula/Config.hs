module Semantics.TruthTables.ChooseForFormula.Config where

import LogicTasks.Config (
  BaseConfig(..),
  PickConfig(..),
  CnfConfig(..),
  FormulaConfig(..)
  )

import Test.Hspec
import Util.VerifyConfig

-- Weight 0.33
task08 :: PickConfig
task08 = PickConfig
  { formulaConfig = FormulaCnf $ CnfConfig
    { baseConf = BaseConfig
      { minClauseLength = 3
      , maxClauseLength = 3
      , usedLiterals = "ABCD"
      }
    , minClauseAmount = 2
    , maxClauseAmount = 2
    }
  , amountOfOptions = 3
  , percentTrueEntries = Nothing
  , extraText = Nothing
  , printSolution = True
  }

-- Weight 0.25
task19 :: PickConfig
task19 = PickConfig
  { formulaConfig = FormulaCnf $ CnfConfig
    { baseConf = BaseConfig
        { minClauseLength = 3
        , maxClauseLength = 3
        , usedLiterals = "ABCDE"
        }
    , minClauseAmount = 4
    , maxClauseAmount = 4
    }
  , amountOfOptions = 4
  , percentTrueEntries = Nothing
  , extraText = Nothing
  , printSolution = True
  }

spec :: Spec
spec = do
  describe "task08" $ verifyFormulaConfig (formulaConfig task08)
  describe "task19" $ verifyFormulaConfig (formulaConfig task19)
