module Semantics.TruthTables.FindMistakes.Config where

import LogicTasks.Config (
  BaseConfig(..),
  DecideConfig(..),
  CnfConfig(..),
  FormulaConfig(..)
  )
import Control.OutputCapable.Blocks (english, german, translations)
import Test.Hspec
import Util.VerifyConfig

-- Weight 0.34
task09 :: DecideConfig
task09 = DecideConfig
  { formulaConfig = FormulaCnf $ CnfConfig
    { baseConf = BaseConfig
      { minClauseLength = 2
      , maxClauseLength = 2
      , usedLiterals = "ABCD"
      }
    , minClauseAmount = 3
    , maxClauseAmount = 3
    }
  , percentageOfChanged = 40
  , percentTrueEntries = Nothing
  , extraText = Nothing
  , printSolution = True
  }

-- Weight 0.4
task11 :: DecideConfig
task11 = DecideConfig
  { formulaConfig = FormulaCnf $ CnfConfig
    { baseConf = BaseConfig
      { minClauseLength = 2
      , maxClauseLength = 3
      , usedLiterals = "ABCD"
      }
    , minClauseAmount = 4
    , maxClauseAmount = 4
    }
  , percentageOfChanged = 40
  , percentTrueEntries = Nothing
  , extraText = Just $ translations $ do
      german "Sie haben nur 2 Versuche, die Aufgabe zu lösen."
      english "You have 2 attempts to solve this task."
  , printSolution = True
  }

spec :: Spec
spec = do
  describe "task09" $ verifyFormulaConfig (formulaConfig task09)
  describe "task11" $ verifyFormulaConfig (formulaConfig task11)
