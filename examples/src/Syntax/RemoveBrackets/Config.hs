module Syntax.RemoveBrackets.Config where

import Tasks.SuperfluousBrackets.Config (
  SuperfluousBracketsConfig(..), checkSuperfluousBracketsConfig,
  )
import Tasks.SynTree.Config (
  SynTreeConfig(..),
  )
import Control.OutputCapable.Blocks (english, german, translations)
import Test.Hspec
import Util.VerifyConfig

-- Weight 0.33
task02 :: SuperfluousBracketsConfig
task02 = SuperfluousBracketsConfig
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
  , superfluousBracketPairs = 3
  , extraText = Just $ translations $ do
      german "Hinweis: Es sollen ALLE nicht nötigen Klammern entfernt werden. Nicht nur die wegen Assoziativität überflüssigen!"
      english "Hint: You need to remove ALL unnecessary pairs of brackets. Not just the ones that are not needed due to associativity."
  , printSolution = True
  }


-- Weight 0.33
task05 :: SuperfluousBracketsConfig
task05 = SuperfluousBracketsConfig
  { syntaxTreeConfig = SynTreeConfig
    { minNodes = 12
    , maxNodes = 15
    , minDepth = 4
    , maxDepth = 6
    , availableAtoms = "ABCDEF"
    , minAmountOfUniqueAtoms = 6
    , allowArrowOperators = True
    , maxConsecutiveNegations = 2
    , minUniqueBinOperators = 3
    }
  , superfluousBracketPairs = 4
  , extraText = Just $ translations $ do
      german "Hinweis: Es sollen ALLE nicht nötigen Klammern entfernt werden. Nicht nur die wegen Assoziativität überflüssigen!"
      english "Hint: You need to remove ALL unnecessary pairs of brackets. Not just the ones that are not needed due to associativity."
  , printSolution = True
  }

spec :: Spec
spec = do
  describe "task02" $ verifyConfig task02 checkSuperfluousBracketsConfig
  describe "task05" $ verifyConfig task05 checkSuperfluousBracketsConfig
