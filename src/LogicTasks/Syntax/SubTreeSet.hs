{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.SubTreeSet where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  ($=<<),
  english,
  german,
  translate,
  localise,
  translations,
  )
import Data.List (nub, sort)
import Data.Set (fromList, isSubsetOf, toList)
import qualified Data.Set (map)
import Data.Maybe (isNothing, fromJust)
import LogicTasks.Helpers (extra, focus, instruct, keyHeading, reject, basicOpKey, arrowsKey)
import Tasks.SubTree.Config (checkSubTreeConfig, SubTreeInst(..), SubTreeConfig(..))
import Trees.Types (FormulaAnswer(..))
import Trees.Print (display, transferToPicture)
import Trees.Helpers
import Control.Monad (when, unless)
import LogicTasks.Syntax.TreeToFormula (cacheTree)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (for_)
import Formula.Parsing.Delayed (Delayed, withDelayed, displayParseError, withDelayedSucceeding)
import Formula.Parsing (Parse(..))


description :: OutputCapable m => SubTreeInst -> LangM m
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

    paragraph $ indent $ do
      translate $ do
        english "For example, if ¬(A ∨ (B ∧ C)) is the given formula and two subformulas are required, then a correct solution is:"
        german "Ist z.B. ¬(A ∨ (B ∧ C)) die gegebene Formel und es werden zwei Teilformeln gesucht, dann ist die folgende Lösung korrekt:"
      translatedCode $ flip localise $ translations exampleCode
      pure ()

    keyHeading
    basicOpKey unicodeAllowed
    when showArrowOperators arrowsKey

    extra addText
    pure ()
      where
        exampleCode | unicodeAllowed = do
                      german "[ A ∨ (B ∧ C), B und C ]"
                      english "[ A ∨ (B ∧ C), B and C ]"
                    | otherwise      = do
                      german "[ A oder (B und C), B und C ]"
                      english "[ A or (B and C), B and C ]"


verifyInst :: OutputCapable m => SubTreeInst -> LangM m
verifyInst _ = pure ()



verifyConfig :: OutputCapable m => SubTreeConfig -> LangM m
verifyConfig = checkSubTreeConfig



start :: [FormulaAnswer]
start = [FormulaAnswer Nothing]


partialGrade :: OutputCapable m => SubTreeInst -> Delayed [FormulaAnswer] -> LangM m
partialGrade inst = (partialGrade' inst `withDelayed` parser) displayParseError

partialGrade' :: OutputCapable m => SubTreeInst -> [FormulaAnswer] -> LangM m
partialGrade' SubTreeInst{..} fs
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

    | otherwise = pure ()
  where
    amount = fromIntegral $ length $ nub fs
    literals = sort $ nub $ concatMap (collectLeaves . fromJust . maybeForm) fs
    opsNum = map (numOfOpsInFormula . fromJust . maybeForm) fs
    origLits = sort $ nub $ collectLeaves tree
    origOpsNum = numOfOps tree


completeGrade
  :: (OutputCapable m, MonadIO m)
  => FilePath
  -> SubTreeInst
  -> Delayed [FormulaAnswer]
  -> LangM m
completeGrade path inst = completeGrade' path inst `withDelayedSucceeding` parser

completeGrade'
  :: (OutputCapable m, MonadIO m)
  => FilePath
  -> SubTreeInst
  -> [FormulaAnswer]
  -> LangM m
completeGrade' path SubTreeInst{..} sol = refuseIfWrong $ do
  unless partOfSolution $ do
    instruct $ do
        english "Your solution is incorrect."
        german "Ihre Lösung ist falsch."

  when showSolution $ indent $ do
    instruct $ do
      english ("A possible solution for this task contains " ++ show minInputTrees ++ " of the following subformulas:")
      german ("Eine mögliche Lösung für die Aufgabe beinhaltet " ++ show minInputTrees ++ " der folgenden Teilformeln:")

    for_ (toList correctTrees) $ \x -> do
      code (display x)
      image $=<< liftIO $ cacheTree (transferToPicture x) path
      pure ()

    pure ()
  pure ()
  where
    partOfSolution = fromList (map show sol) `isSubsetOf` Data.Set.map display correctTrees
    refuseIfWrong = if not partOfSolution then refuse else id
