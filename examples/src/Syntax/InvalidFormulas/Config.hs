module Syntax.InvalidFormulas.Config where

import Tasks.LegalProposition.Config (
  LegalPropositionConfig(..), checkLegalPropositionConfig,
  )
import Tasks.SynTree.Config (
  SynTreeConfig(..),
  )
import Test.Hspec
import Util.VerifyConfig
import Control.OutputCapable.Blocks (Language(German))

-- Weight 0.33
task01 :: LegalPropositionConfig
task01 = LegalPropositionConfig
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
  , formulas = 7
  , illegals = 3
  , bracketFormulas = 0
  , extraText = Nothing
  , printSolution = True
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
    , allowArrowOperators = True
    , maxConsecutiveNegations = 2
    , minUniqueBinOperators = 3
    }
  , formulas = 8
  , illegals = 3
  , bracketFormulas = 1
  , extraText = Nothing
  , printSolution = True
  }

spec :: Spec
spec = do
  describe "task01" $ verifyConfig German task01 checkLegalPropositionConfig
  describe "task17" $ verifyConfig German task17 checkLegalPropositionConfig
