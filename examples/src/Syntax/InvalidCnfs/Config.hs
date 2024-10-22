module Syntax.InvalidCnfs.Config where

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

-- Weight 0.33
task07 :: LegalNormalFormConfig
task07 = LegalNormalFormConfig
  { normalFormConfig = NormalFormConfig
    { baseConf = BaseConfig
      { minClauseLength = 2, maxClauseLength = 4, usedLiterals = "ABCD" }
    , minClauseAmount = 2
    , maxClauseAmount = 4
    }
  , formulas = 8
  , illegals = 2
  , includeFormWithJustOneClause = True
  , includeFormWithJustOneLiteralPerClause = False
  , maxStringSize = 30
  , minStringSize = 12
  , allowArrowOperators = True
  , extraText = Nothing
  , printSolution = True
  }

-- Weight 0.25
task18 :: LegalNormalFormConfig
task18 = LegalNormalFormConfig
  { normalFormConfig = NormalFormConfig
    { baseConf = BaseConfig
      { minClauseLength = 2, maxClauseLength = 4, usedLiterals = "ABCD" }
    , minClauseAmount = 2
    , maxClauseAmount = 5
    }
  , formulas = 8
  , illegals = 2
  , includeFormWithJustOneClause = True
  , includeFormWithJustOneLiteralPerClause = True
  , maxStringSize = 50
  , minStringSize = 12
  , allowArrowOperators = False
  , extraText = Nothing
  , printSolution = True
  }

spec :: Spec
spec = do
  describe "task07" $ verifyConfig German task07 checkLegalNormalFormConfig
  describe "task18" $ verifyConfig German task18 checkLegalNormalFormConfig
