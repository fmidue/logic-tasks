{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.SubTreeSet where


import Control.Monad.Output (LangM, OutputMonad, english, german)
import Data.List (nub, sort)
import Data.Set (fromList, isSubsetOf)
import Data.Maybe (isNothing, fromJust)

import LogicTasks.Helpers (example, extra, focus, fullKey, instruct, keyHeading, reject)
import Tasks.SubTree.Config (checkSubTreeConfig, SubTreeInst(..), SubTreeConfig(..))
import Trees.Types (FormulaAnswer(..))
import Trees.Print (display)
import Trees.Helpers


description :: OutputMonad m => SubTreeInst -> LangM m
description SubTreeInst{..} = do
    instruct $ do
      english "Consider the following propositional logic formula:"
      german "Betrachten Sie die folgende aussagenlogische Formel:"

    focus (display tree)

    instruct $ do
      english $ "Find " ++ show minInputTrees ++ " non-atomic subformulas that are contained in it."
      german $ "Finden Sie " ++ show minInputTrees ++ " nicht-atomare Teilformeln, die in dieser Formel enthalten sind."

    instruct $ do
      english "Submit your solution as a list of subformulas."
      german "Geben Sie die Lösung als eine Liste der Teilformeln an."

    instruct $ do
      english "Remove bracket pairs which only serve to enclose an entire subformula you provide, and do not add any additional brackets."
      german "Entfernen Sie dabei Klammerpaare, die eine angegebene Teilformel komplett umschließen, und fügen Sie keine zusätzlichen Klammern hinzu."

    example "[ A or (B and C), B and C ]" $ do
      english "For example, if ¬(A ∨ (B ∧ C)) is the given formula and two subformulas are required, then a correct solution is:"
      german "Ist z.B. ¬(A ∨ (B ∧ C)) die gegebene Formel und es werden zwei Teilformeln gesucht, dann ist die folgende Lösung korrekt:"

    keyHeading
    fullKey

    extra addText
    pure ()


verifyInst :: OutputMonad m => SubTreeInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputMonad m => SubTreeConfig -> LangM m
verifyConfig = checkSubTreeConfig



start :: [FormulaAnswer]
start = [FormulaAnswer Nothing]



partialGrade :: OutputMonad m => SubTreeInst -> [FormulaAnswer] -> LangM m
partialGrade SubTreeInst{..} fs
    | any (isNothing . maybeForm) fs =
      reject $ do
        english "At least one of your answers is not a valid formula."
        german "Mindestens eine der Antworten ist keine gültige Formel."

    | any (`notElem` origLits) literals =
      reject $ do
        english "At least one subformula contains unknown literals."
        german "Ihre Abgabe beinhaltet mindestens eine Teilformel mit unbekannten Literalen."

    | any (> origOpsNum) opsNum =
      reject $ do
        english "Your solution contains at least one subformula with more logical operators than the original formula."
        german "Ihre Abgabe beinhaltet mindestens eine Teilformel mit mehr logische Operatoren als die ursprüngliche Formel."

    | amount < minInputTrees =
      reject $ do
        english $ "Your solution does not contain enough subformulas. Add " ++ show (minInputTrees - amount) ++ "."
        german $ "Ihre Abgabe beinhaltet nicht genügend Teilformeln. Fügen Sie " ++ show (minInputTrees - amount) ++ " hinzu."

    | otherwise = pure()
  where
    amount = fromIntegral $ length $ nub fs
    literals = sort $ nub $ concatMap (collectLeaves . fromJust . maybeForm) fs
    opsNum = map (numOfOpsInFormula . fromJust . maybeForm) fs
    origLits = sort $ nub $ collectLeaves tree
    origOpsNum = numOfOps tree



completeGrade :: OutputMonad m => SubTreeInst -> [FormulaAnswer] -> LangM m
completeGrade SubTreeInst{..} sol
    | not partOfSolution = reject $ do
      english "Your solution is not correct."
      german "Ihre Abgabe ist keine korrekte Lösung."
    | otherwise = pure()
  where
    partOfSolution = fromList (map show sol) `isSubsetOf` correctFormulas
