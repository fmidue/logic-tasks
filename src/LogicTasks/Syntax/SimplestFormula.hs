{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module LogicTasks.Syntax.SimplestFormula where


import Control.Monad.Output (LangM, OutputMonad(..))
import Data.Either (fromRight)
import Data.List (nub, sort)

import LogicTasks.Syntax.Helpers
import Tasks.SuperfluousBrackets.Config (checkSuperfluousBracketsConfig, SuperfluousBracketsConfig(..), SuperfluousBracketsInst(..))
import Tasks.SuperfluousBrackets.Quiz (feedback)
import Trees.Helpers
import Trees.Parsing (formulaParse)
import Trees.Types




description :: OutputMonad m => SuperfluousBracketsInst -> LangM m
description SuperfluousBracketsInst{..} = do
    instruct
      "Consider the following propositional logic formula:"
      "Betrachten Sie die folgende aussagenlogische Formel:"

    focus stringWithSuperfluousBrackets

    instruct
      "Since /\\ and \\/ are associative, it is not necessary to use brackets when combining three or more atoms with the same operator, for example in:"
      "Aufgrund der Assoziativität von /\\ und \\/ müssen Formeln mit drei oder mehr atomaren Aussagen und den gleichen logischen Operatoren nicht geklammert werden, z.B. bei:"

    focus "A /\\ B /\\ C"

    instruct
      "Remove all unnecessary pairs of brackets in the given formula. Give your answer as a propositional formula."
      "Entfernen Sie alle unnötigen Klammer-Paare in der gegebenen Formel. Geben Sie die Lösung in Form einer Aussagenlogischen Formel an."

    instruct
      "For example, if (A \\/ B) is the given formula, then the solution is:"
      "Ist z.B. (A \\/ B) die gegebene Formel, dann ist die folgende Lösung korrekt:"

    focus "A \\/ B"



verifyInst :: OutputMonad m => SuperfluousBracketsInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputMonad m => SuperfluousBracketsConfig -> LangM m
verifyConfig = checkSuperfluousBracketsConfig



start :: PropFormula
start = Atomic ' '



partialGrade :: OutputMonad m => SuperfluousBracketsInst -> PropFormula -> LangM m
partialGrade SuperfluousBracketsInst{..} f
    | any (`notElem` origLits) literals =
      reject
        "Your solution contains unknown literals."
        "Ihre Abgabe beinhaltet unbekannte Literale."

    | any (`notElem` literals) origLits =
      reject
        "Your solution does not contain all literals present in the original formula."
        "Ihre Abgabe beinhaltet nicht alle Literale aus der ursprünglichen Formel."

    | opsNum > origOpsNum =
      reject
        "Your solution contains more logical operators than the original formula."
        "Ihre Abgabe beinhaltet mehr logische Operatoren als die ursprüngliche Formel."

    | opsNum < origOpsNum =
      reject
        "Your solution contains less logical operators than the original formula."
        "Ihre Abgabe beinhaltet weniger logische Operatoren als die ursprüngliche Formel."

    | otherwise = pure()
  where
    tree = formulaToTree f
    literals = sort $ nub $ collectLeaves tree
    opsNum = numOfOps tree
    origTree = fromRight (Leaf ' ') $ formulaParse stringWithSuperfluousBrackets
    origLits = sort $ nub $ collectLeaves origTree
    origOpsNum = numOfOps origTree



completeGrade :: OutputMonad m => SuperfluousBracketsInst -> PropFormula -> LangM m
completeGrade inst sol
    | not $ feedback inst sol = reject
      "Your solution is not correct."
      "Ihre Abgabe ist nicht die korrekte Lösung"
    | otherwise = pure()