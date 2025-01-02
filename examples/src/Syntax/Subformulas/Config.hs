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
import Control.OutputCapable.Blocks (Language(German,English))
import qualified Data.Map as Map (fromList)
import Data.Map (Map)

listToFM :: Ord k => [(k, a)] -> Map k a
listToFM = Map.fromList

-- 2024: Weight 0.3
task04 :: SubTreeConfig
task04 = SubTreeConfig
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
  , allowSameSubTree = False
  , subTreeAmount = 3
  , extraText = Just (listToFM
                      [ (English, "It does not matter in which order the formulas appear in the list.")
                      , (German, "Es spielt keine Rolle, in welcher Reihenfolge die Formeln in der Liste stehen.") {- german -}
                      ])
  , printSolution = True
  , offerUnicodeInput = True
  }

spec :: Spec
spec = do
  describe "task04" $ verifyConfig German task04 checkSubTreeConfig
