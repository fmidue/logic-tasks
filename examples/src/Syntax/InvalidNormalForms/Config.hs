module Syntax.InvalidNormalForms.Config where

import LogicTasks.Config (
  BaseConfig(..),
  NormalFormConfig(..),
  )
import Tasks.LegalNormalForm.Config (
  LegalNormalFormConfig(..), checkLegalNormalFormConfig,
  )
import Test.Hspec
import Util.VerifyConfig
import Control.OutputCapable.Blocks (Language(German))

-- 2024: Weight 0.25, used for CNFs
task08 :: LegalNormalFormConfig
task08 = LegalNormalFormConfig
  { normalFormConfig = NormalFormConfig
    { baseConf = BaseConfig
      { minClauseLength = 2, maxClauseLength = 4, usedAtoms = "ABCD" }
    , minClauseAmount = 2
    , maxClauseAmount = 4
    , percentPosLiterals = (0,100)
    }
  , formulas = 8
  , illegals = 4
  , includeFormWithJustOneClause = True
  , includeFormWithJustOneLiteralPerClause = False
  , maxStringSize = 30
  , minStringSize = 12
  , allowArrowOperators = True
  , printDetailedSolution = Just True
  , extraText = Nothing
  }

-- 2024: Weight 0.25, used for DNFs
task09 :: LegalNormalFormConfig
task09 = LegalNormalFormConfig
  { normalFormConfig = NormalFormConfig
    { baseConf = BaseConfig
      { minClauseLength = 2, maxClauseLength = 4, usedAtoms = "ABCD" }
    , minClauseAmount = 2
    , maxClauseAmount = 4
    , percentPosLiterals = (0,100)
    }
  , formulas = 8
  , illegals = 3
  , includeFormWithJustOneClause = False
  , includeFormWithJustOneLiteralPerClause = True
  , maxStringSize = 30
  , minStringSize = 12
  , allowArrowOperators = False
  , printDetailedSolution = Just True
  , extraText = Nothing
  }

-- 2023: Weight 0.25
task18 :: LegalNormalFormConfig
task18 = LegalNormalFormConfig
  { normalFormConfig = NormalFormConfig
    { baseConf = BaseConfig
      { minClauseLength = 2, maxClauseLength = 4, usedAtoms = "ABCD" }
    , minClauseAmount = 2
    , maxClauseAmount = 5
    , percentPosLiterals = (0,100)
    }
  , formulas = 8
  , illegals = 2
  , includeFormWithJustOneClause = True
  , includeFormWithJustOneLiteralPerClause = True
  , maxStringSize = 50
  , minStringSize = 12
  , allowArrowOperators = False
  , extraText = Nothing
  , printDetailedSolution = Just True
  }

spec :: Spec
spec = do
  describe "task08" $ verifyConfig German task08 checkLegalNormalFormConfig
  describe "task09" $ verifyConfig German task09 checkLegalNormalFormConfig
  describe "task18" $ verifyConfig German task18 checkLegalNormalFormConfig
