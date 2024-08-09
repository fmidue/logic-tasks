module Semantics.TruthTables.ChooseForFormula.Config where

import LogicTasks.Config (
  PickConfig(..),
  )

import Test.Hspec
import Util.VerifyConfig
import Tasks.SynTree.Config (defaultSynTreeConfig, checkSynTreeConfig)

-- Weight 0.33
task08 :: PickConfig
task08 = PickConfig
  { syntaxTreeConfig = defaultSynTreeConfig -- TODO: change this to a suiting config
  , amountOfOptions = 3
  , percentTrueEntries = Nothing
  , extraText = Nothing
  , printSolution = True
  }

-- Weight 0.25
task19 :: PickConfig
task19 = PickConfig
  { syntaxTreeConfig = defaultSynTreeConfig -- TODO: change this to a suiting config
  , amountOfOptions = 4
  , percentTrueEntries = Nothing
  , extraText = Nothing
  , printSolution = True
  }

spec :: Spec
spec = do
  describe "task08" $ verifyConfig (syntaxTreeConfig task08) checkSynTreeConfig
  describe "task19" $ verifyConfig (syntaxTreeConfig task19) checkSynTreeConfig
