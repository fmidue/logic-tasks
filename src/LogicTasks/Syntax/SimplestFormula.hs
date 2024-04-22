{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.SimplestFormula where


import Control.Monad.Output (LangM, OutputMonad, english, german, paragraph, translate, GenericOutputMonad (refuse))
import Data.List (nub, sort)
import Data.Maybe (isNothing, fromJust)

import LogicTasks.Helpers (basicOpKey, example, extra, focus, instruct, reject, arrowsKey)
import Tasks.SuperfluousBrackets.Config (
    checkSuperfluousBracketsConfig,
    SuperfluousBracketsConfig(..),
    SuperfluousBracketsInst(..)
    )
import Trees.Helpers
import Trees.Types
import Control.Monad (when)
import Formula.Parsing.Delayed (Delayed, withDelayed)
import Formula.Parsing (Parse(..))
import Trees.Parsing()




description :: OutputMonad m => SuperfluousBracketsInst -> LangM m
description SuperfluousBracketsInst{..} = do
    instruct $ do
      english "Consider the following propositional logic formula:"
      german "Betrachten Sie die folgende aussagenlogische Formel:"

    focus stringWithSuperfluousBrackets

    instruct $ do
      english "Since ∧ and ∨ are associative, it is not necessary to use brackets when combining three or more atoms with the same operator, for example in:"
      german "Aufgrund der Assoziativität von ∧ und ∨ müssen Formeln mit drei oder mehr atomaren Aussagen und den gleichen logischen Operatoren nicht geklammert werden, z.B. bei:"

    focus "A ∧ B ∧ C"

    instruct $ do
      english "Similarly brackets are not necessary for one or more consecutive negations directly in front of an atom, for example in:"
      german "Genauso sind Klammern bei einer oder mehreren Negationen direkt vor einer atomaren Aussage nicht nötig, z.B. bei"

    focus "¬¬A"

    instruct $ do
      english "Remove all unnecessary pairs of brackets in the given formula. Give your answer as a propositional logic formula."
      german "Entfernen Sie alle unnötigen Klammer-Paare in der gegebenen Formel. Geben Sie die Lösung in Form einer Aussagenlogischen Formel an."

    example "A ∨ B" $ do
      english "For example, if (A ∨ B) is the given formula, then the solution is:"
      german "Ist z.B. (A ∨ B) die gegebene Formel, dann ist die folgende Lösung korrekt:"

    paragraph $ translate $ do
      german "Sie können dafür die Ausgangsformel in die Abgabe kopieren und unnötige Klammern entfernen, oder die folgenden Schreibweisen nutzen:"
      english "You can copy the original formula into the solution box and remove unnecessary brackets or use the following syntax:"
    basicOpKey
    when showArrowOperators arrowsKey

    extra addText
    pure ()


verifyInst :: OutputMonad m => SuperfluousBracketsInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputMonad m => SuperfluousBracketsConfig -> LangM m
verifyConfig = checkSuperfluousBracketsConfig



start :: FormulaAnswer
start = FormulaAnswer Nothing


partialGrade :: OutputMonad m => SuperfluousBracketsInst -> Delayed FormulaAnswer -> LangM m
partialGrade inst = partialGrade' inst `withDelayed` parser

partialGrade' :: OutputMonad m => SuperfluousBracketsInst -> FormulaAnswer -> LangM m
partialGrade' SuperfluousBracketsInst{..} f
    | isNothing $ maybeForm f =
      reject $ do
        english "Your submission is empty."
        german "Sie haben keine Formel angegeben."

    | any (`notElem` correctLits) literals =
      reject $ do
        english "Your solution contains unknown literals."
        german "Ihre Abgabe beinhaltet unbekannte Literale."

    | any (`notElem` literals) correctLits =
      reject $ do
        english "Your solution does not contain all literals present in the original formula."
        german "Ihre Abgabe beinhaltet nicht alle Literale aus der ursprünglichen Formel."

    | opsNum > correctOpsNum =
      reject $ do
        english "Your solution contains more logical operators than the original formula."
        german "Ihre Abgabe beinhaltet mehr logische Operatoren als die ursprüngliche Formel."

    | opsNum < correctOpsNum =
      reject $ do
        english "Your solution contains less logical operators than the original formula."
        german "Ihre Abgabe beinhaltet weniger logische Operatoren als die ursprüngliche Formel."

    | otherwise = pure()
  where
    pForm = fromJust $ maybeForm f
    literals = sort $ nub $ collectLeaves pForm
    opsNum = numOfOpsInFormula pForm
    correctLits = sort $ nub $ collectLeaves tree
    correctOpsNum = numOfOps tree

completeGrade :: OutputMonad m => SuperfluousBracketsInst -> Delayed FormulaAnswer -> LangM m
completeGrade inst = completeGrade' inst `withDelayed` parser

completeGrade' :: OutputMonad m => SuperfluousBracketsInst -> FormulaAnswer -> LangM m
completeGrade' inst sol
    | show (fromJust (maybeForm sol)) /= simplestString inst = refuse $ do
      instruct $ do
        english "Your solution is incorrect."
        german "Ihre Lösung ist falsch."

      when (showSolution inst) $ do
        example (simplestString inst) $ do
          english "The solution for this task is:"
          german "Die Lösung für die Aufgabe ist:"

      pure ()
    | otherwise = pure()
