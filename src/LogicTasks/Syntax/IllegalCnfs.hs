{-# LANGUAGE RecordWildCards #-}
module LogicTasks.Syntax.IllegalCnfs where


import Control.Monad.Output (LangM, OutputMonad (..))
import Data.List (nub, sort)
import Data.Set (toList)

import LogicTasks.Syntax.Helpers
import Tasks.LegalCNF.Config(LegalCNFConfig(..), LegalCNFInst(..), checkLegalCNFConfig)




description :: OutputMonad m => LegalCNFInst -> LangM m
description LegalCNFInst{..} = do
    instruct
      "Consider the following propositional logic formulae:"
      "Betrachten Sie die folgenden aussagenlogischen Formeln:"

    focus $ unlines $ indexed formulaStrings

    instruct
      "Which of these formulae are not given in conjunctive normal form (cnf)?"
      "Welche dieser Formeln sind nicht in konjunktiver Normalform (cnf) angegeben?"

    instruct
      "Enter a list containing the indices of the non-cnf formulae to submit your answer."
      "Geben Sie eine Liste der Indices aller nicht cnf-Formeln als Ihre Lösung an."

    example
      "For example, if only choices 2 and 3 are non-cnf formulae, then the solution is:"
      "Sind beispielsweise nur Auswahlmöglichkeiten 2 und 3 keine cnf-Formeln, dann ist diese Lösung korrekt:"
      "[2,3]"




verifyInst :: OutputMonad m => LegalCNFInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputMonad m => LegalCNFConfig -> LangM m
verifyConfig = checkLegalCNFConfig

start :: [Int]
start = []



partialGrade :: OutputMonad m => LegalCNFInst -> [Int] -> LangM m
partialGrade LegalCNFInst{..} sol
    | invalidIndex = reject
      "At least one index in the list does not exist."
      "Mindestens einer der Indices existiert nicht."

    | wrongAmount = reject
      "The amount of indices is wrong."
      "Die Anzahl der Indices ist falsch."

    | otherwise = pure()
  where
    nubSol = nub sol
    correct = toList serialsOfWrong
    invalidIndex = any (`notElem` [1..length formulaStrings]) nubSol
    wrongAmount = length nubSol /= length correct



completeGrade :: OutputMonad m => LegalCNFInst -> [Int] -> LangM m
completeGrade LegalCNFInst{..} sol
    | wrongSolution = reject
      "Your solution is incorrect."
      "Ihre Lösung ist falsch."

    | otherwise = pure()
  where
    nubSol = nub sol
    wrongSolution = sort nubSol /= sort (toList serialsOfWrong)
