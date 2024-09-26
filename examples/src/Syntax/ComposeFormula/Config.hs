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
import Control.OutputCapable.Blocks (Language(German))
import qualified Data.Map as Map (fromList)


small :: ComposeFormulaConfig
small = ComposeFormulaConfig
  { syntaxTreeConfig = SynTreeConfig
    { minNodes = 7
    , maxNodes = 9
    , minDepth = 3
    , maxDepth = 4
    , availableAtoms = "ABCD"
    , minAmountOfUniqueAtoms = 4
    , binOpFrequencies = Map.fromList
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
    , binOpFrequencies = Map.fromList
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
  , extraHintsOnAssociativity = True
  , extraText = Nothing
  , printSolution = True
  , offerUnicodeInput = False
  }

spec :: Spec
spec = do
  describe "small" $ verifyConfig German small checkComposeFormulaConfig
  describe "medium" $ verifyConfig German medium checkComposeFormulaConfig
