module Syntax.Subformulas.Config where

import Test.Hspec
import Tasks.SubTree.Config (
  SubTreeConfig(..), checkSubTreeConfig,
  )
import Tasks.SynTree.Config (
  SynTreeConfig(..)
  )
import Trees.Types (BinOp(..))
import Util.VerifyConfig
import Control.OutputCapable.Blocks (Language(German))
import qualified Data.Map as Map (fromList)

medium :: SubTreeConfig
medium = SubTreeConfig
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
  , allowSameSubTree = False
  , minSubTrees = 3
  , extraText = Nothing
  , printSolution = True
  , offerUnicodeInput = False
  }

spec :: Spec
spec = do
  describe "medium" $ verifyConfig German medium checkSubTreeConfig
