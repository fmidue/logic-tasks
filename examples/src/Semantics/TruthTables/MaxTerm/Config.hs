module Semantics.TruthTables.MaxTerm.Config where

import LogicTasks.Config (
  BaseConfig(..),
  MinMaxConfig (..),
  CnfConfig (..),
  )
import Test.Hspec
import LogicTasks.Util (checkCnfConf)
import Util.VerifyConfig

-- Weight 0.33
-- Type: Max
unused01 :: MinMaxConfig
unused01 = MinMaxConfig
  { cnfConf = CnfConfig
    { baseConf = BaseConfig
      { minClauseLength = 2
      , maxClauseLength = 2
      , usedLiterals = "ABC"
      }
    , minClauseAmount = 3
    , maxClauseAmount = 3
    }
  , percentTrueEntries = Just (50, 70)
  , extraText = Nothing
  , printSolution = True
  }

-- Weight 0.34
-- Type: Max
unused02 :: MinMaxConfig
unused02 = MinMaxConfig
  { cnfConf = CnfConfig
    { baseConf = BaseConfig
      { minClauseLength = 2
      , maxClauseLength = 3
      , usedLiterals = "ABCD"
      }
    , minClauseAmount = 3
    , maxClauseAmount = 4
    }
  , percentTrueEntries = Just (50, 70)
  , extraText = Nothing
  , printSolution = True
  }

spec :: Spec
spec = do
  describe "unused01" $ verifyConfig (cnfConf unused01) checkCnfConf
  describe "unused02" $ verifyConfig (cnfConf unused02) checkCnfConf
