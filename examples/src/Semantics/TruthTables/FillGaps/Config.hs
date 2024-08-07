module Semantics.TruthTables.FillGaps.Config where

import LogicTasks.Config (
  FillConfig (..),
  )
import Test.Hspec
import Util.VerifyConfig
import Tasks.SynTree.Config (defaultSynTreeConfig, checkSynTreeConfig)

-- Weight 0.34
task06 :: FillConfig
task06 = FillConfig
  { syntaxTreeConfig = defaultSynTreeConfig -- TODO: change this to a suiting config
  , percentageOfGaps = 60
  , percentTrueEntries = Just (30, 70)
  , extraText = Nothing
  , printSolution = True
  }

-- Weight 0.25
task20 :: FillConfig
task20 = FillConfig
  { syntaxTreeConfig = defaultSynTreeConfig -- TODO: change this to a suiting config
  , percentageOfGaps = 40
  , percentTrueEntries = Just (35, 65)
  , extraText = Nothing
  , printSolution = True
  }

spec :: Spec
spec = do
  describe "task06" $ verifyConfig (syntaxTreeConfig task06) checkSynTreeConfig
  describe "task20" $ verifyConfig (syntaxTreeConfig task20) checkSynTreeConfig
