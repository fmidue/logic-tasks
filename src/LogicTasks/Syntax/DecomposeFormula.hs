{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.DecomposeFormula where


import Control.Monad.Output (
  LangM,
  OutputMonad, german, english,
  GenericOutputMonad (paragraph, indent, translatedCode, refuse, image), translate, localise, translations, ($=<<)
  )

import LogicTasks.Helpers (extra, example, instruct, keyHeading, basicOpKey, arrowsKey, reject)
import Trees.Types (TreeFormulaAnswer(..))
import Tasks.DecomposeFormula.Config (DecomposeFormulaInst(..), DecomposeFormulaConfig, checkDecomposeFormulaConfig)
import Trees.Print (display, transferToPicture)
import Control.Monad (when, unless)
import Data.Maybe (isNothing, fromJust)
import Trees.Helpers (collectLeaves, collectUniqueBinOpsInSynTree, swapKids)
import Data.Containers.ListUtils (nubOrd)
import Control.Monad.Cont (MonadIO (liftIO))
import LogicTasks.Syntax.TreeToFormula (cacheTree)
import Formula.Parsing (Parse(parser))
import Formula.Parsing.Delayed (Delayed, withDelayed)




description :: OutputMonad m => DecomposeFormulaInst -> LangM m
description DecomposeFormulaInst{..} = do

    example (display tree) $ do
      english "The following formula is given:"
      german "Die folgende Formel ist gegeben:"

    instruct $ do
      english "Create the syntax tree for this formula and swap the two subtrees at the root. "
      english "Finally, enter the formula for the resulting syntax tree."
      german "Erstellen Sie den Syntaxbaum zu dieser Formel und tauschen Sie die beiden Teilbäume an der Wurzel. "
      german "Geben Sie anschließend die Formel für den entstandenen Syntaxbaum ein."

    instruct $ do
      english "(You are allowed to add arbitrarily many additional pairs of brackets in the formula.)"
      german "(In der Formel dürfen Sie beliebig viele zusätzliche Klammerpaare hinzufügen.)"

    when addExtraHintsOnAssociativity $ instruct $ do
        english "Remark: Do not try to use associativity in order to omit brackets in this task."
        german "Hinweis: Sie dürfen bei dieser Aufgabe nicht Klammern durch Verwendung von Assoziativität weglassen."

    keyHeading
    basicOpKey
    arrowsKey

    paragraph $ indent $ do
      translate $ do
        english "A solution attempt could look like this: "
        german "Ein Lösungsversuch könnte beispielsweise so aussehen: "
      translatedCode $ flip localise $ translations $ do
        english "(A or not B) and C"
        german "(A oder nicht B) und C"
      pure ()

    extra addText
    pure ()


verifyInst :: OutputMonad m => DecomposeFormulaInst -> LangM m
verifyInst _ = pure ()



verifyConfig :: OutputMonad m => DecomposeFormulaConfig -> LangM m
verifyConfig = checkDecomposeFormulaConfig



start :: TreeFormulaAnswer
start = TreeFormulaAnswer Nothing


partialGrade :: OutputMonad m => DecomposeFormulaInst -> Delayed TreeFormulaAnswer -> LangM m
partialGrade inst = partialGrade' inst `withDelayed` parser

partialGrade' :: OutputMonad m => DecomposeFormulaInst -> TreeFormulaAnswer -> LangM m
partialGrade' DecomposeFormulaInst{..} sol = do

  when (isNothing solTree) $ reject $ do
    english "You did not submit a solution."
    german "Die Abgabe ist leer."

  when (any (`notElem` origLiterals) solLiterals) $ reject $ do
    english "Your solution contains unknown literals."
    german "Ihre Abgabe beinhaltet unbekannte Literale."

  unless (length origLiterals == length solLiterals) $ reject $ do
    english "Your solution does not contain all literals present in the original formula."
    german "Ihre Abgabe beinhaltet nicht alle Literale aus der ursprünglichen Formel."

  unless (length origOperators == length solOperators) $ reject $ do
    english "Your solution does not contain the right amount of different operators."
    german "Ihre Abgabe beinhaltet nicht die richtige Anzahl an unterschiedlichen Operatoren."

  pure ()
    where solTree = maybeTree sol
          origLiterals = nubOrd $ collectLeaves tree
          solLiterals = nubOrd $ collectLeaves $ fromJust solTree
          origOperators = collectUniqueBinOpsInSynTree tree
          solOperators = collectUniqueBinOpsInSynTree $ fromJust solTree



completeGrade :: (OutputMonad m, MonadIO m) => FilePath -> DecomposeFormulaInst -> Delayed TreeFormulaAnswer -> LangM m
completeGrade path inst = completeGrade' path inst `withDelayed` parser

completeGrade' :: (OutputMonad m, MonadIO m) => FilePath -> DecomposeFormulaInst -> TreeFormulaAnswer -> LangM m
completeGrade' path DecomposeFormulaInst{..} sol
  | solTree /= swappedTree = refuse $ do
    instruct $ do
      english "Your solution is not correct."
      german "Ihre Abgabe ist nicht die korrekte Lösung."

    instruct $ do
      english "The original syntax tree looks like this:"
      german "Der originale Syntaxbaum sieht so aus:"

    image $=<< liftIO $ cacheTree (transferToPicture tree) path

    instruct $ do
      english "The syntax tree for your entered formula looks like this:"
      german "Der Syntaxbaum für Ihre eingegebene Formel sieht so aus:"

    image $=<< liftIO $ cacheTree (transferToPicture solTree) path

    when showSolution $ do
      example (show swappedTree) $ do
        english "The solution for this task is:"
        german "Die Lösung für die Aufgabe ist:"

      instruct $ do
        english "The corresponding syntax tree looks like this:"
        german "Der zugehörige Syntaxbaum sieht so aus:"

      image $=<< liftIO $ cacheTree (transferToPicture swappedTree) path

      pure ()

    pure ()
  | otherwise = pure ()
    where solTree = fromJust $ maybeTree sol
          swappedTree = swapKids tree

