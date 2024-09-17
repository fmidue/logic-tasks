{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module LogicTasks.Syntax.IllegalCnfs where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (refuse),
  LangM,
  OutputCapable,
  english,
  german,
  )
import Data.List (nub, sort)
import Data.Set (toList)
import LogicTasks.Helpers
import Tasks.LegalCNF.Config(LegalCNFConfig(..), LegalCNFInst(..), checkLegalCNFConfig)
import Control.Monad (when)





description :: OutputCapable m => LegalCNFInst -> LangM m
description LegalCNFInst{..} = do
    instruct $ do
      english "Consider the following propositional logic formulas:"
      german "Betrachten Sie die folgenden aussagenlogischen Formeln:"

    focus $ unlines $ indexed formulaStrings

    instruct $ do
      english "Which of these formulas are given in conjunctive normal form (cnf)?"
      german "Welche dieser Formeln sind in konjunktiver Normalform (KNF) angegeben?"

    instruct $ do
      english "Enter a list containing the indices of the cnf formulas to submit your answer."
      german "Geben Sie eine Liste der Indizes aller in KNF vorliegenden Formeln als Ihre Lösung an."

    example "[2,3]" $ do
      english "For example, if only choices 2 and 3 are cnf formulas, then the solution is:"
      german "Liegen beispielsweise nur Auswahlmöglichkeiten 2 und 3 in KNF vor, dann ist diese Lösung korrekt:"

    extra addText

    pure ()



verifyInst :: OutputCapable m => LegalCNFInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputCapable m => LegalCNFConfig -> LangM m
verifyConfig = checkLegalCNFConfig



start :: [Int]
start = []



partialGrade :: OutputCapable m => LegalCNFInst -> [Int] -> LangM m
partialGrade LegalCNFInst{..} sol
    | invalidIndex = reject $ do
      english "At least one index in the list does not exist."
      german "Mindestens einer der Indizes existiert nicht."

    | otherwise = pure()
  where
    nubSol = nub sol
    invalidIndex = any (`notElem` [1..length formulaStrings]) nubSol



completeGrade :: OutputCapable m => LegalCNFInst -> [Int] -> LangM m
completeGrade inst sol
    | wrongSolution = refuse $ do
      instruct $ do
        english "Your solution is incorrect."
        german "Ihre Lösung ist falsch."

      when (showSolution inst) $ do
        example (show serialsOfRight) $ do
          english "A possible solution for this task is:"
          german "Eine mögliche Lösung für die Aufgabe ist:"

      pure ()

    | otherwise = pure()
  where
    wrongSolution = sort (nub sol) /= sort serialsOfRight
    serialsOfRight = filter (`notElem` toList (serialsOfWrong inst)) [1..length (formulaStrings inst)]
