module Semantics.TruthTables.MaxTerm.Config where

import LogicTasks.Config (
  BaseConfig(..),
  MinMaxConfig (..),
  NormalFormConfig (..),
  )
import Test.Hspec
import LogicTasks.Util (checkNormalFormConfig)
import Util.VerifyConfig
import Control.OutputCapable.Blocks (Language(German))

-- Weight 0.33
-- Type: Max
unused01 :: MinMaxConfig
unused01 = MinMaxConfig
  { cnfConf = NormalFormConfig
    { baseConf = BaseConfig
      { minClauseLength = 2
      , maxClauseLength = 2
      , usedLiterals = "ABC"
      }
    , minClauseAmount = 3
    , maxClauseAmount = 3
    }
  , percentTrueEntries = Just (50, 70)
  , extraText = Nothing
  , printSolution = True
  , offerUnicodeInput = False
  }

-- Weight 0.34
-- Type: Max
unused02 :: MinMaxConfig
unused02 = MinMaxConfig
  { cnfConf = NormalFormConfig
    { baseConf = BaseConfig
      { minClauseLength = 2
      , maxClauseLength = 3
      , usedLiterals = "ABCD"
      }
    , minClauseAmount = 3
    , maxClauseAmount = 4
    }
  , percentTrueEntries = Just (50, 70)
  , extraText = Nothing
  , printSolution = True
  , offerUnicodeInput = False
  }

spec :: Spec
spec = do
  describe "unused01" $ verifyConfig German (cnfConf unused01) checkNormalFormConfig
  describe "unused02" $ verifyConfig German (cnfConf unused02) checkNormalFormConfig
