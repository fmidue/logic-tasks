module Semantics.TruthTables.ChooseForFormula.Config where

import LogicTasks.Config (
  BaseConfig(..),
  PickConfig(..),
  CnfConfig(..),
  )
import LogicTasks.Util (checkCnfConf)

import Test.Hspec
import Util.VerifyConfig
import Control.OutputCapable.Blocks (Language(German))

-- Weight 0.33
task08 :: PickConfig
task08 = PickConfig
  { cnfConf = CnfConfig
    { baseConf = BaseConfig
      { minClauseLength = 3
      , maxClauseLength = 3
      , usedLiterals = "ABCD"
      }
    , minClauseAmount = 2
    , maxClauseAmount = 2
    }
  , amountOfOptions = 3
  , pickCnf = False
  , extraText = Nothing
  , printSolution = True
  }

-- Weight 0.25
task19 :: PickConfig
task19 = PickConfig
  { cnfConf = CnfConfig
    { baseConf = BaseConfig
        { minClauseLength = 3
        , maxClauseLength = 3
        , usedLiterals = "ABCDE"
        }
    , minClauseAmount = 4
    , maxClauseAmount = 4
    }
  , amountOfOptions = 4
  , pickCnf = False
  , extraText = Nothing
  , printSolution = True
  }

spec :: Spec
spec = do
  describe "task08" $ verifyConfig German (cnfConf task08) checkCnfConf
  describe "task19" $ verifyConfig German (cnfConf task19) checkCnfConf
