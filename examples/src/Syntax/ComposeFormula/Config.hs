module Syntax.ComposeFormula.Config where

import Prelude hiding (and, or)
import Tasks.ComposeFormula.Config (
  ComposeFormulaConfig(..), checkComposeFormulaConfig, TreeDisplayMode (TreeDisplay),
  )
import Tasks.SynTree.Config (
  SynTreeConfig(..),
  OperatorFrequencies(..)
  )

import Test.Hspec
import Util.VerifyConfig
import Control.OutputCapable.Blocks (Language(German))


small :: ComposeFormulaConfig
small = ComposeFormulaConfig
  { syntaxTreeConfig = SynTreeConfig
    { minNodes = 7
    , maxNodes = 9
    , minDepth = 3
    , maxDepth = 4
    , availableAtoms = "ABCD"
    , minAmountOfUniqueAtoms = 4
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
  , treeDisplayModes = (TreeDisplay, TreeDisplay)
  , extraHintsOnAssociativity = True
  , extraText = Nothing
  , printSolution = True
  , offerUnicodeInput = False
  }

medium :: ComposeFormulaConfig
medium = ComposeFormulaConfig
  { syntaxTreeConfig = SynTreeConfig
    { minNodes = 10
    , maxNodes = 14
    , minDepth = 4
    , maxDepth = 6
    , availableAtoms = "ABCDE"
    , minAmountOfUniqueAtoms = 5
    , operatorFrequencies = OperatorFrequencies
      { and = 1
      , or = 1
      , impl = 1
      , backImpl = 1
      , equi = 1
      , neg = 1
      }
    , maxConsecutiveNegations = 2
    , minUniqueBinOperators = 2
    }
  , treeDisplayModes = (TreeDisplay, TreeDisplay)
  , extraHintsOnAssociativity = True
  , extraText = Nothing
  , printSolution = True
  , offerUnicodeInput = False
  }

spec :: Spec
spec = do
  describe "small" $ verifyConfig German small checkComposeFormulaConfig
  describe "medium" $ verifyConfig German medium checkComposeFormulaConfig
