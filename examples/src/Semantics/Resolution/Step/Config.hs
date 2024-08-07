module Semantics.Resolution.Step.Config where

import LogicTasks.Config (
  BaseConfig(..),
  StepConfig(..),
  )
import LogicTasks.Util (checkBaseConf)
import Test.Hspec
import Util.VerifyConfig

-- Weight 0.2
task12 :: StepConfig
task12 =
  StepConfig
    { baseConf = BaseConfig
      { minClauseLength = 2
      , maxClauseLength = 2
      , usedLiterals = "ABCD"
      }
    , extraText = Nothing
    , printSolution = True
    }

-- Weight 0.25
task13 :: StepConfig
task13 = task12

-- Weight 0.25
task14 :: StepConfig
task14 =
  StepConfig
    { baseConf = BaseConfig
      { minClauseLength = 3
      , maxClauseLength = 3
      , usedLiterals = "ABCD"
      }
    , extraText = Nothing
    , printSolution = True
    }

spec :: Spec
spec = do
  describe "task12" $ verifyConfig (baseConf task12) checkBaseConf
  describe "task14" $ verifyConfig (baseConf task14) checkBaseConf
