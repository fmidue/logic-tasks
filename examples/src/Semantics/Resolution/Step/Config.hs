module Semantics.Resolution.Step.Config where

import LogicTasks.Config (
  BaseConfig(..),
  StepConfig(..),
  )
import LogicTasks.Util (checkBaseConf)
import Test.Hspec
import Util.VerifyConfig
import Control.OutputCapable.Blocks (Language(German))

-- 2025: Weight 0.1
task15 :: StepConfig
task15 =
  StepConfig
    { baseConf = BaseConfig
      { minClauseLength = 2
      , maxClauseLength = 2
      , usedAtoms = "ABCD"
      }
    , useSetNotation = True
    , printSolution = True
    , extraText = Nothing
    , offerUnicodeInput = True
    }

-- 2023: Weight 0.25
task13 :: StepConfig
task13 = StepConfig
    { baseConf = BaseConfig
      { minClauseLength = 2
      , maxClauseLength = 2
      , usedAtoms = "ABCD"
      }
    , useSetNotation = False
    , extraText = Nothing
    , printSolution = True
    , offerUnicodeInput = False
    }

-- 2025: Weight 0.1
task16 :: StepConfig
task16 =
  StepConfig
    { baseConf = BaseConfig
      { minClauseLength = 3
      , maxClauseLength = 3
      , usedAtoms = "ABCD"
      }
    , useSetNotation = True
    , printSolution = True
    , extraText = Nothing
    , offerUnicodeInput = True
    }

spec :: Spec
spec = do
  describe "task13" $ verifyConfig German (baseConf task13) checkBaseConf
  describe "task15" $ verifyConfig German (baseConf task15) checkBaseConf
  describe "task16" $ verifyConfig German (baseConf task16) checkBaseConf
