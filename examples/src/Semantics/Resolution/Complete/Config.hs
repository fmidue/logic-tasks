module Semantics.Resolution.Complete.Config where

import LogicTasks.Config (
  BaseConfig(..),
  ResolutionConfig(..),
  )
import Test.Hspec
import Util.VerifyConfig
import LogicTasks.Util (checkBaseConf)
import Control.OutputCapable.Blocks (Language(German))

-- 2025: Weight 0.5
task17 :: ResolutionConfig
task17 = ResolutionConfig
    { baseConf = BaseConfig
      { minClauseLength = 2
      , maxClauseLength = 3
      , usedAtoms = "ABCD"
      }
    , minSteps = 3
    , printFeedbackImmediately = True
    , useSetNotation = True
    , printSolution = False
    , extraText = Nothing
    , offerUnicodeInput = True
    }

-- 2025: Weight 0.5
task18 :: ResolutionConfig
task18 = ResolutionConfig
  { baseConf = BaseConfig
      { minClauseLength = 4
      , maxClauseLength = 5
      , usedAtoms = "ABCDE"
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
  describe "task17" $ verifyConfig German (baseConf task17) checkBaseConf
  describe "task18" $ verifyConfig German (baseConf task18) checkBaseConf
