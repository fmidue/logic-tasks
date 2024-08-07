module Semantics.TruthTables.FillGaps.Config where

import LogicTasks.Config (
  BaseConfig(..),
  FillConfig (..),
  CnfConfig (..),
  )
import Test.Hspec
import LogicTasks.Util (checkCnfConf)
import Util.VerifyConfig

-- Weight 0.34
task06 :: FillConfig
task06 = FillConfig
  { cnfConf = CnfConfig
    { baseConf = BaseConfig
      { minClauseLength = 2
      , maxClauseLength = 2
      , usedLiterals = "ABC"
      }
    , minClauseAmount = 3
    , maxClauseAmount = 3
    }
  , percentageOfGaps = 60
  , percentTrueEntries = Just (30, 70)
  , extraText = Nothing
  , printSolution = True
  }

-- Weight 0.25
task20 :: FillConfig
task20 = FillConfig
  { cnfConf = CnfConfig
    { baseConf = BaseConfig
      { minClauseLength = 3
      , maxClauseLength = 3
      , usedLiterals = "ABCDE"
      }
    , minClauseAmount = 4
    , maxClauseAmount = 4
    }
  , percentageOfGaps = 40
  , percentTrueEntries = Just (35, 65)
  , extraText = Nothing
  , printSolution = True
  }

spec :: Spec
spec = do
  describe "task06" $ verifyConfig (cnfConf task06) checkCnfConf
  describe "task20" $ verifyConfig (cnfConf task20) checkCnfConf
