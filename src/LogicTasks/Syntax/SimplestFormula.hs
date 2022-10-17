{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module LogicTasks.Syntax.SimplestFormula where

import Tasks.SuperfluousBrackets.Config (defaultSuperfluousBracketsConfig, SuperfluousBracketsConfig(..), SuperfluousBracketsInst(..))
import Tasks.SuperfluousBrackets.Quiz (generateSuperfluousBracketsInst, feedback)
import LogicTasks.Syntax.AppHelp (offerChange, determineBaseConfig, feedbackLoop)
import Test.QuickCheck (generate)

import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  theConfigToUse <- determineSuperfluousBracketsConfig
  putStrLn "\nThe following is the config now used:\n"
  pPrint theConfigToUse
  putStrLn "\nThe following is a random instance generated from it:\n"
  inst@SuperfluousBracketsInst{ simplestString } <- generate . generateSuperfluousBracketsInst $ theConfigToUse
  pPrint inst
  putStrLn "\n This is an important syntax task before you deal with CNF and DNF"
  putStrLn "\n Because of /\\ and \\/ are associative, it is not necessary to use brackets when combining three atoms with same operators /\\ or \\/ for example A/\\B/\\C"
  putStrLn "\n The task will give a formula with redundant brackets, and your mission is to give the simplest form of the formula"
  putStrLn "\n That means delete all unnecessary brackets"
  feedbackLoop (feedback inst) ("The sample solution is: " ++ simplestString)

determineSuperfluousBracketsConfig :: IO SuperfluousBracketsConfig
determineSuperfluousBracketsConfig = do
  putStrLn "\nThe following is the default config:\n"
  pPrint defaultSuperfluousBracketsConfig
  let SuperfluousBracketsConfig{..} = defaultSuperfluousBracketsConfig
  syntaxTreeConfig' <- determineBaseConfig syntaxTreeConfig
  superfluousBracketPairs' <- offerChange "superfluousBracketPairs" superfluousBracketPairs
  let newConfig = defaultSuperfluousBracketsConfig {syntaxTreeConfig = syntaxTreeConfig', superfluousBracketPairs = superfluousBracketPairs'}
  return newConfig
