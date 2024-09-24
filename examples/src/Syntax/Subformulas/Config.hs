module Syntax.Subformulas.Config where

import Prelude hiding (and, or)
import Test.Hspec
import Tasks.SubTree.Config (
  SubTreeConfig(..), checkSubTreeConfig,
  )
import Tasks.SynTree.Config (
  SynTreeConfig(..),
  OperatorFrequencies(..)
  )
import Util.VerifyConfig
import Control.OutputCapable.Blocks (Language(German))

medium :: SubTreeConfig
medium = SubTreeConfig
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
  , allowSameSubTree = False
  , minSubTrees = 3
  , extraText = Nothing
  , printSolution = True
  , offerUnicodeInput = False
  }

spec :: Spec
spec = do
  describe "medium" $ verifyConfig German medium checkSubTreeConfig
