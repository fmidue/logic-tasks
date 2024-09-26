module Syntax.RemoveBrackets.Config where

import Tasks.SuperfluousBrackets.Config (
  SuperfluousBracketsConfig(..), checkSuperfluousBracketsConfig,
  )
import Tasks.SynTree.Config (
  SynTreeConfig(..)
  )
import Trees.Types (BinOp(..))
import Control.OutputCapable.Blocks (english, german, translations, Language (German))
import Test.Hspec
import Util.VerifyConfig
import qualified Data.Map as Map (fromList)

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
  , superfluousBracketPairs = 3
  , extraText = Just $ translations $ do
      german "Hinweis: Es sollen ALLE nicht nötigen Klammern entfernt werden. Nicht nur die wegen Assoziativität überflüssigen!"
      english "Hint: You need to remove ALL unnecessary pairs of brackets. Not just the ones that are not needed due to associativity."
  , printSolution = True
  , offerUnicodeInput = False
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
    , binOpFrequencies = Map.fromList
      [ (And, 1)
      , (Or, 1)
      , (Impl, 1)
      , (BackImpl, 1)
      , (Equi, 1)
      ]
    , negOpFrequency = 1
    , maxConsecutiveNegations = 2
    , minUniqueBinOperators = 3
    }
  , superfluousBracketPairs = 4
  , extraText = Just $ translations $ do
      german "Hinweis: Es sollen ALLE nicht nötigen Klammern entfernt werden. Nicht nur die wegen Assoziativität überflüssigen!"
      english "Hint: You need to remove ALL unnecessary pairs of brackets. Not just the ones that are not needed due to associativity."
  , printSolution = True
  , offerUnicodeInput = False
  }

spec :: Spec
spec = do
  describe "task02" $ verifyConfig German task02 checkSuperfluousBracketsConfig
  describe "task05" $ verifyConfig German task05 checkSuperfluousBracketsConfig
