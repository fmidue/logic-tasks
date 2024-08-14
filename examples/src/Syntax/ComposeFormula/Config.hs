module Syntax.ComposeFormula.Config where

import Tasks.ComposeFormula.Config (
  ComposeFormulaConfig(..), checkComposeFormulaConfig, TreeDisplayMode (TreeDisplay),
  )
import Tasks.SynTree.Config (
  SynTreeConfig(..),
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
    , allowArrowOperators = False
    , maxConsecutiveNegations = 2
    , minUniqueBinOperators = 2
    }
  , treeDisplayModes = (TreeDisplay, TreeDisplay)
  , extraHintsOnAssociativity = True
  , extraText = Nothing
  , printSolution = True
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
    , allowArrowOperators = True
    , maxConsecutiveNegations = 2
    , minUniqueBinOperators = 2
    }
  , treeDisplayModes = (TreeDisplay, TreeDisplay)
  , extraHintsOnAssociativity = True
  , extraText = Nothing
  , printSolution = True
  }

spec :: Spec
spec = do
  describe "small" $ verifyConfig German small checkComposeFormulaConfig
  describe "medium" $ verifyConfig German medium checkComposeFormulaConfig
