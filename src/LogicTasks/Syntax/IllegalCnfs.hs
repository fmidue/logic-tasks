{-# LANGUAGE RecordWildCards #-}
module LogicTasks.Syntax.IllegalCnfs where

import Test.QuickCheck (generate)
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Pretty.Simple (pPrint)
import LogicTasks.Syntax.AppHelp (offerChange, feedbackLoop)
import Tasks.LegalCNF.Quiz(generateLegalCNFInst, feedback)
import Tasks.LegalCNF.Config(LegalCNFConfig(..), LegalCNFInst(..), defaultLegalCNFConfig, checkLegalCNFConfig)
import Config(BaseConfig(..), CnfConfig(..))

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  theConfigToUse <- determineLegalCNFConfig
  putStrLn "\nThe following is the config now used:\n"
  pPrint theConfigToUse
  putStrLn "\nThe following is a random instance generated from it:\n"
  inst@LegalCNFInst{..} <- generate . generateLegalCNFInst $ theConfigToUse
  pPrint inst
  putStrLn "In this task there are some propositional logic formulas, your task is to give the set of ordinal numbers of the illegal conjunctive normal form (CNF) in propositional logic formulas"
  putStrLn "The input form is {serial number1, serial number2,..}"
  feedbackLoop (feedback inst) ("The sample solution is " ++ show serialsOfWrong)

determineLegalCNFConfig :: IO LegalCNFConfig
determineLegalCNFConfig = do
    putStrLn "\nThe following is the default config:\n"
    pPrint defaultLegalCNFConfig
    let LegalCNFConfig {cnfConfig = cnfConfig@CnfConfig {baseConf = baseConf@BaseConfig{..}, ..}, ..} = defaultLegalCNFConfig
    minClauseLength' <- offerChange "minClauseLength" minClauseLength
    maxClauseLength' <- offerChange "maxClauseLength" maxClauseLength
    usedLiterals' <- offerChange "usedLiterals" usedLiterals
    minClauseAmount' <- offerChange "minClauseAmount" minClauseAmount
    maxClauseAmount' <- offerChange "maxClauseAmount" maxClauseAmount
    formulas' <- offerChange "formulas" formulas
    illegals' <- offerChange "illegals" illegals
    externalGenFormulas' <- offerChange "externalGenFormulas" externalGenFormulas
    includeFormWithJustOneClause' <- offerChange "includeFormWithJustOneClause" includeFormWithJustOneClause
    includeFormWithJustOneLiteralPerClause' <- offerChange "includeFormWithJustOneLiteralPerClause" includeFormWithJustOneLiteralPerClause
    maxStringSize' <- offerChange "maxStringSize" maxStringSize
    minStringSize' <- offerChange "minStringSize" minStringSize
    allowArrowOperators' <- offerChange "allowArrowOperators" allowArrowOperators
    let newBaseConfig = baseConf {minClauseLength = minClauseLength', maxClauseLength = maxClauseLength', usedLiterals = usedLiterals'}
        newCNFConfig = cnfConfig {baseConf = newBaseConfig, minClauseAmount = minClauseAmount', maxClauseAmount = maxClauseAmount'}
        newConfig = defaultLegalCNFConfig{
            formulas = formulas'
          , illegals = illegals'
          , cnfConfig = newCNFConfig
          , includeFormWithJustOneLiteralPerClause = includeFormWithJustOneLiteralPerClause'
          , includeFormWithJustOneClause = includeFormWithJustOneClause'
          , externalGenFormulas = externalGenFormulas'
          , maxStringSize = maxStringSize'
          , minStringSize = minStringSize'
          , allowArrowOperators = allowArrowOperators'}
    case checkLegalCNFConfig newConfig of
      Nothing ->
        return newConfig
      Just problem -> do
        putStrLn $ "This didn't go well. Here is the problem: " ++ problem
        putStrLn "You should try again."
        determineLegalCNFConfig

