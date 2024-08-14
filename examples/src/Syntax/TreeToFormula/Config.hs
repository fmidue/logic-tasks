module Syntax.TreeToFormula.Config where

import Test.Hspec

import Tasks.SynTree.Config (
  SynTreeConfig(..),
  )
import Tasks.TreeToFormula.Config (
  TreeToFormulaConfig(..),checkTreeToFormulaConfig,
  )
import Util.VerifyConfig

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
    , allowArrowOperators = False
    , maxConsecutiveNegations = 2
    , minUniqueBinOperators = 2
    }
  , extraHintsOnSemanticEquivalence = True
  , extraText = Nothing
  , printSolution = True
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
    , allowArrowOperators = True
    , maxConsecutiveNegations = 3
    , minUniqueBinOperators = 2
    }
  , extraHintsOnSemanticEquivalence = True
  , extraText = Nothing
  , printSolution = True
  }

-- Weight 0.4
task10 :: TreeToFormulaConfig
task10 = task04

spec :: Spec
spec = do
  describe "task03" $ verifyConfig task03 checkTreeToFormulaConfig
  describe "task04" $ verifyConfig task04 checkTreeToFormulaConfig
