{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.IllegalFormulas where


import Control.Monad.Output (LangM, OutputMonad(..))
import Data.List (nub, sort)
import Data.Set (toList)

import LogicTasks.Syntax.Helpers
import Tasks.LegalProposition.Config (LegalPropositionInst(..), LegalPropositionConfig(..), checkLegalPropositionConfig)




description :: OutputMonad m => LegalPropositionInst -> LangM m
description LegalPropositionInst{..} = do
    instruct
      "Consider the following propositional formulae:"
      "Betrachten Sie die folgenden aussagenlogischen Formeln:"

    focus $ unlines $ indexed pseudoFormulas

    instruct
      "Some of these formulae are syntactically incorrect. Which of these formulae are invalid?"
      "Einige dieser Formeln enthalten syntaktische Fehler. Geben Sie an, welche Formeln nicht korrekt sind."

    instruct
      "Enter a list containing the indices of the invalid formulae to submit your answer."
      "Geben Sie eine Liste der Indices aller syntaktisch falschen Formeln als Ihre Lösung an."

    example
      "For example, if only choices 2 and 3 are incorrect, then the solution is:"
      "Sind beispielsweise nur Auswahlmöglichkeiten 2 und 3 falsch, dann ist diese Lösung korrekt:"
      "[2,3]"




verifyInst :: OutputMonad m => LegalPropositionInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputMonad m => LegalPropositionConfig -> LangM m
verifyConfig = checkLegalPropositionConfig

start :: [Int]
start = []



partialGrade :: OutputMonad m => LegalPropositionInst -> [Int] -> LangM m
partialGrade LegalPropositionInst{..} sol
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
    invalidIndex = any (`notElem` [1..length pseudoFormulas]) nubSol
    wrongAmount = length nubSol /= length correct



completeGrade :: OutputMonad m => LegalPropositionInst -> [Int] -> LangM m
completeGrade LegalPropositionInst{..} sol
    | wrongSolution = reject
      "Your solution is incorrect."
      "Ihre Lösung ist falsch."

    | otherwise = pure()
  where
    nubSol = nub sol
    wrongSolution = sort nubSol /= sort (toList serialsOfWrong)
