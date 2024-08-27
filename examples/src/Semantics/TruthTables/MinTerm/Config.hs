module Semantics.TruthTables.MinTerm.Config where

import LogicTasks.Config (
  BaseConfig(..),
  MinMaxConfig (..),
  CnfConfig (..),
  )
import Test.Hspec
import LogicTasks.Util (checkCnfConf)
import Util.VerifyConfig
import Control.OutputCapable.Blocks (Language(German))

-- Weight 0.2
-- Type: Min
unused03 :: MinMaxConfig
unused03 = MinMaxConfig
  { cnfConf = CnfConfig
    { baseConf = BaseConfig
      { minClauseLength = 2
      , maxClauseLength = 3
      , usedLiterals = "ABCD"
      }
    , minClauseAmount = 2
    , maxClauseAmount = 3
    }
  , percentTrueEntries = Just (55, 70)
  , extraText = Nothing
  , printSolution = True
  }

spec :: Spec
spec = do
  describe "unused03" $ verifyConfig German (cnfConf unused03) checkCnfConf
