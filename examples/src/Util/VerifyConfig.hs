{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Util.VerifyConfig where

import Control.OutputCapable.Blocks (LangM, OutputCapable, Language(German))
import Test.Hspec
import Type.Reflection
import Config (FormulaConfig(..))
import LogicTasks.Util (checkCnfConf)
import Tasks.SynTree.Config (checkSynTreeConfig)
import Control.OutputCapable.Blocks.Debug (checkConfigWith)

verifyConfig :: Language -> a -> (forall m. OutputCapable m => a -> LangM m) -> Spec
verifyConfig lang config checker = itIsValid $ checkConfigWith lang config checker `shouldReturn` True

verifyFormulaConfig :: FormulaConfig -> Spec
verifyFormulaConfig (FormulaCnf cnfCfg) = verifyConfig German cnfCfg checkCnfConf
verifyFormulaConfig (FormulaDnf cnfCfg) = verifyConfig German cnfCfg checkCnfConf
verifyFormulaConfig (FormulaArbitrary syntaxTreeConfig) = verifyConfig German syntaxTreeConfig checkSynTreeConfig

noChecker :: forall a. Typeable a => a -> Spec
noChecker _ = itIsValid $ pendingWith $ "no checker for " ++ show (typeRep @a)

itIsValid :: Example a => a -> SpecWith (Arg a)
itIsValid = it "is a valid configuration"
