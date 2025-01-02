module Semantics.Resolution.Complete.Config where

import LogicTasks.Config (
  BaseConfig(..),
  ResolutionConfig(..),
  )
import Test.Hspec
import Util.VerifyConfig
import LogicTasks.Util (checkBaseConf)
import Control.OutputCapable.Blocks (Language(German))

-- 2024: Weight 0.5
task16 :: ResolutionConfig
task16 = ResolutionConfig
    { baseConf = BaseConfig
      { minClauseLength = 2
      , maxClauseLength = 3
      , usedLiterals = "ABCD"
      }
    , minSteps = 3
    , printFeedbackImmediately = True
    , useSetNotation = True
    , printSolution = False
    , extraText = Nothing
    , offerUnicodeInput = True
    }

-- 2024: Weight 0.5
task17 :: ResolutionConfig
task17 = ResolutionConfig
  { baseConf = BaseConfig
      { minClauseLength = 4
      , maxClauseLength = 5
      , usedLiterals = "ABCDE"
      }
  , minSteps = 4
  , printFeedbackImmediately = True
  , useSetNotation = True
  , printSolution = False
  , extraText = Nothing
  , offerUnicodeInput = True
  }

spec :: Spec
spec = do
  describe "task16" $ verifyConfig German (baseConf task16) checkBaseConf
  describe "task17" $ verifyConfig German (baseConf task17) checkBaseConf
