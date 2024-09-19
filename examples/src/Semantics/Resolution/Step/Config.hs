module Semantics.Resolution.Step.Config where

import LogicTasks.Config (
  BaseConfig(..),
  StepConfig(..),
  )
import LogicTasks.Util (checkBaseConf)
import Test.Hspec
import Util.VerifyConfig
import Control.OutputCapable.Blocks (Language(German))

-- Weight 0.2
task12 :: StepConfig
task12 =
  StepConfig
    { baseConf = BaseConfig
      { minClauseLength = 2
      , maxClauseLength = 2
      , usedLiterals = "ABCD"
      }
    , useSetNotation = False
    , extraText = Nothing
    , printSolution = True
    , offerUnicodeInput = False
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
    , useSetNotation = False
    , extraText = Nothing
    , printSolution = True
    , offerUnicodeInput = False
    }

spec :: Spec
spec = do
  describe "task12" $ verifyConfig German (baseConf task12) checkBaseConf
  describe "task14" $ verifyConfig German (baseConf task14) checkBaseConf
