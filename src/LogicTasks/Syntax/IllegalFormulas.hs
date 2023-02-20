{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.IllegalFormulas where


import Control.Monad.Output (LangM, OutputMonad(..), english, german)
import Data.List (nub, sort)
import Data.Set (toList)

import LogicTasks.Helpers
import Tasks.LegalProposition.Config (LegalPropositionInst(..), LegalPropositionConfig(..), checkLegalPropositionConfig)




description :: OutputMonad m => LegalPropositionInst -> LangM m
description LegalPropositionInst{..} = do
    instruct $ do
      english "Consider the following propositional (pseudo) formulae:"
      german "Betrachten Sie die folgenden aussagenlogischen (Pseudo-)Formeln:"

    focus $ unlines $ indexed pseudoFormulas

    instruct $ do
      english "Some of these are syntactically incorrect. Which of these formulae are invalid?"
      german "Einige davon enthalten syntaktische Fehler. Geben Sie an, welche Formeln nicht korrekt sind."

    instruct $ do
      english "Enter a list containing the indices of the invalid formulae to submit your answer."
      german "Geben Sie eine Liste der Indices aller syntaktisch falschen Formeln als Ihre Lösung an."

    example "[2,3]" $ do
      english "For example, if only choices 2 and 3 are incorrect, then the solution is:"
      german "Sind beispielsweise nur Auswahlmöglichkeiten 2 und 3 falsch, dann ist diese Lösung korrekt:"



verifyInst :: OutputMonad m => LegalPropositionInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputMonad m => LegalPropositionConfig -> LangM m
verifyConfig = checkLegalPropositionConfig



start :: [Int]
start = []



partialGrade :: OutputMonad m => LegalPropositionInst -> [Int] -> LangM m
partialGrade LegalPropositionInst{..} sol
    | invalidIndex = reject $ do
      english "At least one index in the list does not exist."
      german "Mindestens einer der Indices existiert nicht."

    | otherwise = pure()
  where
    nubSol = nub sol
    invalidIndex = any (`notElem` [1..length pseudoFormulas]) nubSol



completeGrade :: OutputMonad m => LegalPropositionInst -> [Int] -> LangM m
completeGrade inst sol
    | wrongSolution = reject $ do
      english "Your solution is incorrect."
      german "Ihre Lösung ist falsch."

    | otherwise = pure()
  where
    wrongSolution = sort (nub sol) /= sort (toList $ serialsOfWrong inst)
