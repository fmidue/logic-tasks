module Semantics.TruthTables.FindMistakes.Config where

import LogicTasks.Config (
  BaseConfig(..),
  DecideConfig(..),
  NormalFormConfig(..),
  FormulaConfig(..)
  )
import Test.Hspec
import Tasks.SynTree.Config (
  SynTreeConfig(..)
  )
import Trees.Types (BinOp(..))
import Util.VerifyConfig
import qualified Data.Map as Map (fromList)
import Data.Map (Map)

listToFM :: Ord k => [(k, a)] -> Map k a
listToFM = Map.fromList

-- 2025: Weight 0.3
task11 :: DecideConfig
task11 = DecideConfig
  { formulaConfig =
      FormulaCnf (NormalFormConfig
                   { baseConf = BaseConfig
                     { minClauseLength = 2
                     , maxClauseLength = 2
                     , usedAtoms = "ABCD"
                     }
                   , minClauseAmount = 3
                   , maxClauseAmount = 3
                   })
  , percentageOfChanged = 40
  , percentTrueEntries = (30, 70)
  , printSolution = True
  , extraText = Nothing
  }

-- 2025: Weight 0.3
task13 :: DecideConfig
task13 = DecideConfig
  { formulaConfig =
      FormulaArbitrary (SynTreeConfig
                        { minNodes = 10
                        , maxNodes = 14
                        , minDepth = 4
                        , maxDepth = 6
                        , availableAtoms = "ABCD"
                        , minAmountOfUniqueAtoms = 4
                        , binOpFrequencies = listToFM
                          [ (And, 1)
                          , (Or, 1)
                          , (Impl, 1)
                          , (BackImpl, 0)
                          , (Equi, 1)
                          ]
                        , negOpFrequency = 1
                        , maxConsecutiveNegations = 2
                        , minUniqueBinOperators = 4
                        })
  , percentageOfChanged = 30
  , percentTrueEntries = (30, 70)
  , printSolution = True
  , extraText = Nothing
  }

spec :: Spec
spec = do
  describe "task11" $ verifyFormulaConfig (formulaConfig task11)
  describe "task13" $ verifyFormulaConfig (formulaConfig task13)
