module Syntax.InvalidCnfs.Config where

import LogicTasks.Config (
  BaseConfig(..),
  CnfConfig(..),
  )
import Tasks.LegalCNF.Config (
  LegalCNFConfig(..), checkLegalCNFConfig,
  )
import Test.Hspec
import Util.VerifyConfig

-- Weight 0.33
task07 :: LegalCNFConfig
task07 = LegalCNFConfig
  { cnfConfig = CnfConfig
    { baseConf = BaseConfig
      { minClauseLength = 2, maxClauseLength = 4, usedLiterals = "ABCD" }
    , minClauseAmount = 2
    , maxClauseAmount = 4
    }
  , formulas = 8
  , externalGenFormulas = 2
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
task18 :: LegalCNFConfig
task18 = LegalCNFConfig
  { cnfConfig = CnfConfig
    { baseConf = BaseConfig
      { minClauseLength = 2, maxClauseLength = 4, usedLiterals = "ABCD" }
    , minClauseAmount = 2
    , maxClauseAmount = 5
    }
  , formulas = 8
  , externalGenFormulas = 2
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
  describe "task07" $ verifyConfig task07 checkLegalCNFConfig
  describe "task18" $ verifyConfig task18 checkLegalCNFConfig
