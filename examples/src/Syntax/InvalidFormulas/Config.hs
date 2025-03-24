module Syntax.InvalidFormulas.Config where

import Tasks.LegalProposition.Config (
  LegalPropositionConfig(..), checkLegalPropositionConfig,
  )
import Tasks.SynTree.Config (
  SynTreeConfig(..)
  )
import Trees.Types (BinOp(..))
import Test.Hspec
import Util.VerifyConfig
import Control.OutputCapable.Blocks (Language(German))
import qualified Data.Map as Map (fromList)
import Data.Map (Map)

listToFM :: Ord k => [(k, a)] -> Map k a
listToFM = Map.fromList

-- 2024: Weight 0.33
task01 :: LegalPropositionConfig
task01 = LegalPropositionConfig
  { syntaxTreeConfig = SynTreeConfig
    { minNodes = 10
    , maxNodes = 12
    , minDepth = 4
    , maxDepth = 5
    , availableAtoms = "ABCDE"
    , minAmountOfUniqueAtoms = 5
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
  , formulas = 7
  , illegals = 3
  , bracketFormulas = 0
  , extraText = Nothing
  , printSolution = Just False
  }

-- Weight 0.25
task17 :: LegalPropositionConfig
task17 = LegalPropositionConfig
  { syntaxTreeConfig = SynTreeConfig
    { minNodes = 12
    , maxNodes = 14
    , minDepth = 4
    , maxDepth = 6
    , availableAtoms = "ABCDEF"
    , minAmountOfUniqueAtoms = 6
    , binOpFrequencies = listToFM
      [ (And, 1)
      , (Or, 1)
      , (Impl, 1)
      , (BackImpl, 0)
      , (Equi, 1)
      ]
    , negOpFrequency = 1
    , maxConsecutiveNegations = 2
    , minUniqueBinOperators = 3
    }
  , formulas = 8
  , illegals = 3
  , bracketFormulas = 1
  , extraText = Nothing
  , printSolution = Just False
  }

spec :: Spec
spec = do
  describe "task01" $ verifyConfig German task01 checkLegalPropositionConfig
  describe "task17" $ verifyConfig German task17 checkLegalPropositionConfig
