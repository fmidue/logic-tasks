{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.IllegalFormulas where

import Tasks.LegalProposition.Config (LegalPropositionInst(..), LegalPropositionConfig(..), defaultLegalPropositionConfig, checkLegalPropositionConfig)
import Tasks.LegalProposition.Quiz (generateLegalPropositionInst, feedback)
import LogicTasks.Syntax.AppHelp (offerChange, determineBaseConfig, feedbackLoop)
import Test.QuickCheck (generate)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  theConfigToUse <- determineLegalPropositionConfig
  putStrLn "\nThe following is the config now used:\n"
  pPrint theConfigToUse
  putStrLn "\nThe following is a random instance generated from it:\n"
  inst@LegalPropositionInst{..} <- generate . generateLegalPropositionInst $ theConfigToUse
  pPrint inst
  putStrLn "In this task there are some Pseudo Formulas, your task is to give the set of ordinal numbers of the illegal formulas in Pseudo Formulas"
  putStrLn "The input form is {serial number1, serial number2,..}"
  feedbackLoop (feedback inst) ("The sample solution is " ++ show serialsOfWrong)

determineLegalPropositionConfig :: IO LegalPropositionConfig
determineLegalPropositionConfig = do
  putStrLn "\nThe following is the default config:\n"
  pPrint defaultLegalPropositionConfig
  let LegalPropositionConfig {..} = defaultLegalPropositionConfig
  syntaxTreeConfig' <- determineBaseConfig syntaxTreeConfig
  formulas' <- offerChange "formulas" formulas
  illegals' <- offerChange "illegals" illegals
  bracketFormulas' <- offerChange "bracketFormulas" bracketFormulas
  let newConfig = defaultLegalPropositionConfig {syntaxTreeConfig = syntaxTreeConfig', formulas = formulas', illegals = illegals', bracketFormulas = bracketFormulas'}
  case checkLegalPropositionConfig newConfig of
    Nothing ->
      return newConfig
    Just problem -> do
      putStrLn $ "This didn't go well. Here is the problem: " ++ problem
      putStrLn "You should try again."
      determineLegalPropositionConfig
