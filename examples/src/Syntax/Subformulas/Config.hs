module Syntax.Subformulas.Config where

import Test.Hspec
import Tasks.SubTree.Config (
  SubTreeConfig(..), checkSubTreeConfig,
  )
import Tasks.SynTree.Config (
  SynTreeConfig(..),
  )
import Util.VerifyConfig

medium :: SubTreeConfig
medium = SubTreeConfig
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
  , allowSameSubTree = False
  , minSubTrees = 3
  , extraText = Nothing
  , printSolution = True
  }

spec :: Spec
spec = do
  describe "medium" $ verifyConfig medium checkSubTreeConfig
