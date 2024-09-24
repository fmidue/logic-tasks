module Syntax.TreeToFormula.Config where

import Prelude hiding (and, or)
import Test.Hspec

import Tasks.SynTree.Config (
  SynTreeConfig(..),
  OperatorFrequencies(..)
  )
import Tasks.TreeToFormula.Config (
  TreeToFormulaConfig(..),checkTreeToFormulaConfig,
  )
import Util.VerifyConfig
import Control.OutputCapable.Blocks (Language(German))

-- Weight 0.34
task03 :: TreeToFormulaConfig
task03 = TreeToFormulaConfig
  { syntaxTreeConfig = SynTreeConfig
    { minNodes = 10
    , maxNodes = 12
    , minDepth = 4
    , maxDepth = 5
    , availableAtoms = "ABCDE"
    , minAmountOfUniqueAtoms = 5
    , operatorFrequencies = OperatorFrequencies
      { and = 1
      , or = 1
      , impl = 0
      , backImpl = 0
      , equi = 0
      , neg = 1
      }
    , maxConsecutiveNegations = 2
    , minUniqueBinOperators = 2
    }
  , extraHintsOnSemanticEquivalence = True
  , extraText = Nothing
  , printSolution = True
  , offerUnicodeInput = False
  }

-- Weight 0.33
task04 :: TreeToFormulaConfig
task04 =  TreeToFormulaConfig
  { syntaxTreeConfig = SynTreeConfig
    { minNodes = 15
    , maxNodes = 18
    , minDepth = 4
    , maxDepth = 9
    , availableAtoms = "ABCDEFG"
    , minAmountOfUniqueAtoms = 7
    , operatorFrequencies = OperatorFrequencies
      { and = 1
      , or = 1
      , impl = 0
      , backImpl = 0
      , equi = 0
      , neg = 1
      }
    , maxConsecutiveNegations = 3
    , minUniqueBinOperators = 2
    }
  , extraHintsOnSemanticEquivalence = True
  , extraText = Nothing
  , printSolution = True
  , offerUnicodeInput = False
  }

-- Weight 0.4
task10 :: TreeToFormulaConfig
task10 = task04

spec :: Spec
spec = do
  describe "task03" $ verifyConfig German task03 checkTreeToFormulaConfig
  describe "task04" $ verifyConfig German task04 checkTreeToFormulaConfig
