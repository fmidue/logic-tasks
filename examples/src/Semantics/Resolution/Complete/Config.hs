module Semantics.Resolution.Complete.Config where

import LogicTasks.Config (
  BaseConfig(..),
  ResolutionConfig(..),
  )
import Test.Hspec
import Util.VerifyConfig
import LogicTasks.Util (checkBaseConf)

-- Weight 0.5
task15 :: ResolutionConfig
task15 = ResolutionConfig
    { baseConf = BaseConfig
      { minClauseLength = 2
      , maxClauseLength = 3
      , usedLiterals = "ABCD"
      }
    , minSteps = 3
    , printFeedbackImmediately = True
    , printSolution = True
    , extraText = Nothing
    }

-- Weight 0.5
task16 :: ResolutionConfig
task16 =  ResolutionConfig
  { baseConf = BaseConfig
      { minClauseLength = 4
      , maxClauseLength = 5
      , usedLiterals = "ABCDE"
      }
  , minSteps = 4
  , printFeedbackImmediately = True
  , printSolution = True
  , extraText = Nothing
  }

spec :: Spec
spec = do
  describe "task15" $ verifyConfig (baseConf task15) checkBaseConf
  describe "task16" $ verifyConfig (baseConf task16) checkBaseConf
