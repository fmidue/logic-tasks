module Semantics.TruthTables.FindMistakes.Config where

import LogicTasks.Config (
  DecideConfig(..),
  )
import Control.OutputCapable.Blocks (english, german, translations)
import Test.Hspec
import Util.VerifyConfig
import Tasks.SynTree.Config (defaultSynTreeConfig, checkSynTreeConfig)

-- Weight 0.34
task09 :: DecideConfig
task09 = DecideConfig
  { syntaxTreeConfig = defaultSynTreeConfig -- TODO: change this to a suiting config
  , percentageOfChanged = 40
  , percentTrueEntries = Nothing
  , extraText = Nothing
  , printSolution = True
  }

-- Weight 0.4
task11 :: DecideConfig
task11 = DecideConfig
  { syntaxTreeConfig = defaultSynTreeConfig -- TODO: change this to a suiting config
  , percentageOfChanged = 40
  , percentTrueEntries = Nothing
  , extraText = Just $ translations $ do
      german "Sie haben nur 2 Versuche, die Aufgabe zu lösen."
      english "You have 2 attempts to solve this task."
  , printSolution = True
  }

spec :: Spec
spec = do
  describe "task09" $ verifyConfig (syntaxTreeConfig task09) checkSynTreeConfig
  describe "task11" $ verifyConfig (syntaxTreeConfig task11) checkSynTreeConfig
