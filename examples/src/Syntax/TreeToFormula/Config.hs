module Syntax.TreeToFormula.Config where

import Test.Hspec

import Tasks.SynTree.Config (
  SynTreeConfig(..)
  )
import Trees.Types (BinOp(..))
import Tasks.TreeToFormula.Config (
  TreeToFormulaConfig(..),checkTreeToFormulaConfig,
  )
import Util.VerifyConfig
import Control.OutputCapable.Blocks (Language(..))
import qualified Data.Map as Map (fromList)
import Data.Map (Map)

listToFM :: Ord k => [(k, a)] -> Map k a
listToFM = Map.fromList

-- 2025: Weight 0.3
task02 :: TreeToFormulaConfig
task02 = TreeToFormulaConfig
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
  , extraText = Just (listToFM
                        [ (German, "Sie dürfen bei dieser Aufgabe nicht Klammern durch Verwendung von Assoziativität weglassen.")
                        , (English, "Do not try to use associativity in order to omit brackets in this task.")
                        ])
  , printSolution = True
  , offerUnicodeInput = True
  }

-- 2023: Weight 0.33
task04 :: TreeToFormulaConfig
task04 =  TreeToFormulaConfig
  { syntaxTreeConfig = SynTreeConfig
    { minNodes = 15
    , maxNodes = 18
    , minDepth = 4
    , maxDepth = 9
    , availableAtoms = "ABCDEFG"
    , minAmountOfUniqueAtoms = 7
    , binOpFrequencies = listToFM
      [ (And, 1)
      , (Or, 1)
      , (Impl, 0)
      , (BackImpl, 0)
      , (Equi, 0)
      ]
    , negOpFrequency = 1
    , maxConsecutiveNegations = 3
    , minUniqueBinOperators = 2
    }
  , extraText = Just (listToFM
                        [ (German, "Es muss die exakte Formel des Syntaxbaums angegeben werden. "
                                ++ "Andere, selbst zu dieser Formel semantisch äquivalente Formeln sind keine korrekte Lösung! "
                                ++ "Sie dürfen bei dieser Aufgabe nicht Klammern durch Verwendung von Assoziativität weglassen.")
                        , (English, "The exact formula of the syntax tree must be given. "
                                ++ "Other formulas that are semantically equivalent to this formula are incorrect solutions! "
                                ++ "Do not try to use associativity in order to omit brackets in this task.")
                        ])
  , printSolution = True
  , offerUnicodeInput = False
  }

-- 2023: Weight 0.4
task10 :: TreeToFormulaConfig
task10 = task04

spec :: Spec
spec = do
  describe "task02" $ verifyConfig German task02 checkTreeToFormulaConfig
  describe "task04" $ verifyConfig German task04 checkTreeToFormulaConfig
