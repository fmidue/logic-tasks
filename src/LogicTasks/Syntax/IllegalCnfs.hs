{-# LANGUAGE RecordWildCards #-}
module LogicTasks.Syntax.IllegalCnfs where


import Control.Monad.Output (LangM, OutputMonad (..), english, german)
import Data.List (nub, sort)
import Data.Set (toList)

import LogicTasks.Helpers
import Tasks.LegalCNF.Config(LegalCNFConfig(..), LegalCNFInst(..), checkLegalCNFConfig)




description :: OutputMonad m => LegalCNFInst -> LangM m
description LegalCNFInst{..} = do
    instruct $ do
      english "Consider the following propositional logic formulae:"
      german "Betrachten Sie die folgenden aussagenlogischen Formeln:"

    focus $ unlines $ indexed formulaStrings

    instruct $ do
      english "Which of these formulae are not given in conjunctive normal form (cnf)?"
      german "Welche dieser Formeln sind nicht in konjunktiver Normalform (cnf) angegeben?"

    instruct $ do
      english "Enter a list containing the indices of the non-cnf formulae to submit your answer."
      german "Geben Sie eine Liste der Indices aller nicht cnf-Formeln als Ihre Lösung an."

    example "[2,3]" $ do
      english "For example, if only choices 2 and 3 are non-cnf formulae, then the solution is:"
      german "Sind beispielsweise nur Auswahlmöglichkeiten 2 und 3 keine cnf-Formeln, dann ist diese Lösung korrekt:"



verifyInst :: OutputMonad m => LegalCNFInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputMonad m => LegalCNFConfig -> LangM m
verifyConfig = checkLegalCNFConfig



start :: [Int]
start = []



partialGrade :: OutputMonad m => LegalCNFInst -> [Int] -> LangM m
partialGrade LegalCNFInst{..} sol
    | invalidIndex = reject $ do
      english "At least one index in the list does not exist."
      german "Mindestens einer der Indices existiert nicht."

    | otherwise = pure()
  where
    nubSol = nub sol
    invalidIndex = any (`notElem` [1..length formulaStrings]) nubSol



completeGrade :: OutputMonad m => LegalCNFInst -> [Int] -> LangM m
completeGrade inst sol
    | wrongSolution = reject $ do
      english "Your solution is incorrect."
      german "Ihre Lösung ist falsch."

    | otherwise = pure()
  where
    wrongSolution = sort (nub sol) /= sort (toList $ serialsOfWrong inst)
