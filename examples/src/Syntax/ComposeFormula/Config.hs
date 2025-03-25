module Syntax.ComposeFormula.Config where

import Tasks.ComposeFormula.Config (
  ComposeFormulaConfig(..), checkComposeFormulaConfig, TreeDisplayMode (TreeDisplay),
  )
import Tasks.SynTree.Config (
  SynTreeConfig(..)
  )
import Trees.Types (BinOp(..))

import Test.Hspec
import Util.VerifyConfig
import Control.OutputCapable.Blocks (Language(..))
import qualified Data.Map as Map (fromList)
import Data.Map (Map)

listToFM :: Ord k => [(k, a)] -> Map k a
listToFM = Map.fromList


small :: ComposeFormulaConfig
small = ComposeFormulaConfig
  { syntaxTreeConfig = SynTreeConfig
    { minNodes = 7
    , maxNodes = 9
    , minDepth = 3
    , maxDepth = 4
    , availableAtoms = "ABCD"
    , minAmountOfUniqueAtoms = 4
    , binOpFrequencies = listToFM
      [ (And, 1)
      , (Or, 1)
      , (Impl, 0)
      , (BackImpl, 0)
      , (Equi, 0)
      ]
    , negOpFrequency = 1
    , maxConsecutiveNegations = 2
    , minUniqueBinOperators = 2
    }
  , treeDisplayModes = (TreeDisplay, TreeDisplay)
  , extraText = Just (listToFM
                        [ (German, "Sie dürfen bei dieser Aufgabe nicht Klammern durch Verwendung von Assoziativität weglassen.")  -- no-spell-check
                        , (English, "Do not try to use associativity in order to omit brackets in this task.")
                        ])
  , printSolution = True
  , offerUnicodeInput = False
  }

-- 2024: Weight 0.33
task03 :: ComposeFormulaConfig
task03 = ComposeFormulaConfig
  { syntaxTreeConfig = SynTreeConfig
    { minNodes = 10
    , maxNodes = 14
    , minDepth = 4
    , maxDepth = 6
    , availableAtoms = "ABCDE"
    , minAmountOfUniqueAtoms = 5
    , binOpFrequencies = listToFM
      [ (And, 1)
      , (Or, 1)
      , (Impl, 1)
      , (BackImpl, 1)
      , (Equi, 1)
      ]
    , negOpFrequency = 1
    , maxConsecutiveNegations = 2
    , minUniqueBinOperators = 2
    }
  , treeDisplayModes = (TreeDisplay, TreeDisplay)
  , extraText = Nothing
  , printSolution = True
  , offerUnicodeInput = True
  }

spec :: Spec
spec = do
  describe "small" $ verifyConfig German small checkComposeFormulaConfig
  describe "task03" $ verifyConfig German task03 checkComposeFormulaConfig
