{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.SimplestFormula where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (refuse, indent, translatedCode),
  LangM,
  OutputCapable,
  english,
  german,
  paragraph,
  translate,
  localise,
  translations,
  )
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
import Formula.Parsing.Delayed (Delayed, parseDelayedWithAndThen, complainAboutMissingParenthesesIfNotFailingOn, withDelayedSucceeding)
import Formula.Parsing (Parse(..), formulaSymbolParser)
import Trees.Parsing()




description :: OutputCapable m => SuperfluousBracketsInst -> LangM m
description SuperfluousBracketsInst{..} = do
    instruct $ do
      english "Consider the following propositional logic formula:"
      german "Betrachten Sie die folgende aussagenlogische Formel:"

    focus stringWithSuperfluousBrackets

    instruct $ do
      english "Since ∧ and ∨ are associative, it is not necessary to use brackets in subformulas with three or more atomic formulas and the same logical operators, for example in:"
      german "Aufgrund der Assoziativität von ∧ und ∨ muss in Teilformeln mit drei oder mehr atomaren Formeln und den gleichen logischen Operatoren nicht geklammert werden, z.B. bei:"

    focus "A ∧ B ∧ C"

    instruct $ do
      english "Similarly, brackets are not necessary for one or more consecutive negations directly in front of an atomic formula, for example in:"
      german "Genauso sind Klammern bei einer oder mehreren Negationen direkt vor einer atomaren Formel nicht nötig, z.B. bei"

    focus "¬¬A"

    instruct $ do
      english "Remove all unnecessary pairs of brackets in the given formula. Give your answer as a propositional logic formula."
      german "Entfernen Sie alle unnötigen Klammer-Paare in der gegebenen Formel. Geben Sie die Lösung in Form einer aussagenlogischen Formel an."

    paragraph $ indent $ do
      translate $ do
        english "For example, if (A ∨ B) is the given formula, then the following solution is correct:"
        german "Ist z.B. (A ∨ B) die gegebene Formel, dann ist die folgende Lösung korrekt:"
      translatedCode $ flip localise $ translations exampleCode
      pure ()

    paragraph $ translate $ do
      german "Sie können dafür die ürsprüngliche Formel in das Abgabefeld kopieren und unnötige Klammern entfernen, oder leer startend die folgenden Schreibweisen nutzen:"
      english "You can copy the original formula into the submission field and remove unnecessary brackets, or start from scratch and use the following syntax:"
    basicOpKey unicodeAllowed
    when showArrowOperators arrowsKey

    extra addText
    pure ()
      where
        exampleCode | unicodeAllowed = do
                      german "A ∨ B"
                      english "A ∨ B"
                    | otherwise      = do
                      german "A oder B"
                      english "A or B"


verifyInst :: OutputCapable m => SuperfluousBracketsInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputCapable m => SuperfluousBracketsConfig -> LangM m
verifyConfig = checkSuperfluousBracketsConfig



start :: FormulaAnswer
start = FormulaAnswer Nothing


partialGrade :: OutputCapable m => SuperfluousBracketsInst -> Delayed FormulaAnswer -> LangM m
partialGrade = parseDelayedWithAndThen parser complainAboutMissingParenthesesIfNotFailingOn formulaSymbolParser . partialGrade'

partialGrade' :: OutputCapable m => SuperfluousBracketsInst -> FormulaAnswer -> LangM m
partialGrade' SuperfluousBracketsInst{..} f
    | isNothing $ maybeForm f =
      reject $ do
        english "Your submission is empty."
        german "Sie haben keine Formel angegeben."

    | any (`notElem` correctAtoms) atoms =
      reject $ do
        english "Your submission contains unknown atomic formulas."
        german "Ihre Abgabe beinhaltet unbekannte atomare Formeln."

    | any (`notElem` atoms) correctAtoms =
      reject $ do
        english "Your submission does not contain all atomic formulas present in the original formula."
        german "Ihre Abgabe beinhaltet nicht alle atomaren Formeln aus der ursprünglichen Formel."

    | opsNum > correctOpsNum =
      reject $ do
        english "Your submission contains more logical operators than the original formula."
        german "Ihre Abgabe beinhaltet mehr logische Operatoren als die ursprüngliche Formel."

    | opsNum < correctOpsNum =
      reject $ do
        english "Your submission contains less logical operators than the original formula."
        german "Ihre Abgabe beinhaltet weniger logische Operatoren als die ursprüngliche Formel."

    | otherwise = pure()
  where
    pForm = fromJust $ maybeForm f
    atoms = sort $ nub $ collectLeaves pForm
    opsNum = numOfOpsInFormula pForm
    correctAtoms = sort $ nub $ collectLeaves tree
    correctOpsNum = numOfOps tree

completeGrade :: OutputCapable m => SuperfluousBracketsInst -> Delayed FormulaAnswer -> LangM m
completeGrade inst = completeGrade' inst `withDelayedSucceeding` parser

completeGrade' :: OutputCapable m => SuperfluousBracketsInst -> FormulaAnswer -> LangM m
completeGrade' inst sol
    | show (fromJust (maybeForm sol)) /= simplestString inst = refuse $ do
      instruct $ do
        english "Your solution is incorrect."
        german "Ihre Lösung ist falsch."

      when (showSolution inst) $ do
        example (simplestString inst) $ do
          english "The solution for this task is:"
          german "Die Lösung für diese Aufgabe ist:"

      pure ()
    | otherwise = pure()
