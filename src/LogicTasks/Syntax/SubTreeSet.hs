{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.SubTreeSet where

import Tasks.SubTree.Config (SubTreeInst(..), SubTreeConfig(..), defaultSubTreeConfig)
import Tasks.SubTree.Quiz (generateSubTreeInst, feedback)
import LogicTasks.Syntax.AppHelp (offerChange, determineBaseConfig, feedbackLoop)
import Test.QuickCheck (generate)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  theConfigToUse <- determineSubTreeConfig
  putStrLn "\nThe following is the config now used:\n"
  pPrint theConfigToUse
  putStrLn "\nThe following is a random instance generated from it:\n"
  inst@SubTreeInst{..} <- generate . generateSubTreeInst $ theConfigToUse
  pPrint inst
  putStrLn ("\nThe task will give a formula and your task is to input a set which at least have " ++ show minInputTrees ++ " non-atomic Formulas")
  putStrLn "\nInput form is {subformula1,subformula2..}"
  putStrLn "\nDo not keep unnecessary parentheses outside subformulas and do not add any additional parentheses"
  feedbackLoop (feedback inst) ("One of the sample solution is " ++ show correctFormulas)

determineSubTreeConfig :: IO SubTreeConfig
determineSubTreeConfig = do
  putStrLn "\nThe following is the default config:\n"
  pPrint defaultSubTreeConfig
  let SubTreeConfig{..} = defaultSubTreeConfig
  syntaxTreeConfig' <- determineBaseConfig syntaxTreeConfig
  allowSameSubTree' <- offerChange "allowSameSubTree" allowSameSubTree
  minSubTrees' <- offerChange "minSubTrees" minSubTrees
  let newConfig = defaultSubTreeConfig {syntaxTreeConfig = syntaxTreeConfig', minSubTrees = minSubTrees', allowSameSubTree = allowSameSubTree'}
  return newConfig
