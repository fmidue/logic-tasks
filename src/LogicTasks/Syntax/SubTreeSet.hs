{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.SubTreeSet where


import Control.Monad.Output (LangM, OutputMonad(..))
import Data.List (nub, sort)

import LogicTasks.Syntax.Helpers
import Tasks.SubTree.Config (checkSubTreeConfig, SubTreeInst(..), SubTreeConfig(..))
import Tasks.SubTree.Quiz (feedback)
import Trees.Types (PropFormula(..))
import Trees.Helpers


description :: OutputMonad m => SubTreeInst -> LangM m
description SubTreeInst{..} = do
    instruct
      "Consider the following propositional logic formula:"
      "Betrachten Sie die folgende aussagenlogische Formel:"

    focus formula

    instruct
      ("Find " ++ show minInputTrees ++ " non atomic subformulae that are contained in it.")
      ("Finden Sie " ++ show minInputTrees ++ " nicht atomare Subformeln, die in dieser Formel enthalten sind.")

    instruct
      "Submit your solution as a list of subformulae."
      "Geben Sie die Lösung als eine Liste der Subformeln an."

    instruct
      "Remove bracket pairs which only serve to enclose entire Subformulae and do not add any additional brackets."
      "Entfernen Sie dabei Klammerpaare, die eine Subformel komplett umschließen und fügen Sie keine zusätzlichen Klammern hinzu."

    example
      "For example, if ~(A \\/ B) is the given formula and one subformula is required, then the solution is:"
      "Ist z.B. ~(A \\/ B) die gegebene Formel und es wird eine Subformel gesucht, dann ist die folgende Lösung korrekt:"
      "[ A \\/ B ]"



verifyInst :: OutputMonad m => SubTreeInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputMonad m => SubTreeConfig -> LangM m
verifyConfig = checkSubTreeConfig



start :: [PropFormula]
start = []



partialGrade :: OutputMonad m => SubTreeInst -> [PropFormula] -> LangM m
partialGrade SubTreeInst{..} fs
    | any (`notElem` origLits) literals =
      reject
        "At least one subformula contains unknown literals."
        "Ihre Abgabe beinhaltet mindestens eine Subformel mit unbekannten Literalen."

    | any (> origOpsNum) opsNum =
      reject
        "Your solution contains at least one subformula with more logical operators than the original formula."
        "Ihre Abgabe beinhaltet mindestens eine Subformel mit mehr logische Operatoren als die ursprüngliche Formel."

    | amount < minInputTrees =
      reject
        ("Your solution does not contain enough subformulae. Add " ++ show (minInputTrees - amount) ++ ".")
        ("Ihre Abgabe beinhaltet nicht genügend Subformeln. Fügen Sie " ++ show (minInputTrees - amount) ++ " hinzu.")

    | otherwise = pure()
  where
    amount = fromIntegral $ length $ nub fs
    formTrees = map formulaToTree fs
    literals = sort $ nub $ concatMap collectLeaves formTrees
    opsNum = map numOfOps formTrees
    origLits = sort $ nub $ collectLeaves tree
    origOpsNum = numOfOps tree



completeGrade :: OutputMonad m => SubTreeInst -> [PropFormula] -> LangM m
completeGrade inst sol
    | not $ feedback inst sol = reject
      "Your solution is not correct."
      "Ihre Abgabe ist nicht die korrekte Lösung"
    | otherwise = pure()
