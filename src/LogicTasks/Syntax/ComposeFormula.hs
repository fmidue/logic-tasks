{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.ComposeFormula where


import Control.Monad.IO.Class(MonadIO (liftIO))
import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  OutputMonad,
  ($=<<),
  english,
  german, translate, localise, translations,
  )
import Data.Maybe (fromJust, isNothing, isJust)

import LogicTasks.Helpers (extra, fullKey, instruct, keyHeading, reject, example)
import Tasks.SynTree.Config (checkSynTreeConfig, SynTreeConfig)
import Trees.Types (TreeFormulaAnswer(..), SynTree (Binary), showOperator)
import Control.Monad (when)
import Trees.Print (transferToPicture, display)
import Tasks.ComposeFormula.Config (ComposeFormulaInst(..))
import Trees.Helpers (collectLeaves, collectUniqueBinOpsInSynTree)
import Data.Containers.ListUtils (nubOrd)
import Formula.Util (isSemanticEqual)
import LogicTasks.Syntax.TreeToFormula (cacheTree)




description :: (OutputMonad m, MonadIO m) => FilePath -> ComposeFormulaInst -> LangM m
description path ComposeFormulaInst{..} = do
    instruct $ do
      english "Imagine that the two displayed trees/formulas are hung below a root node with operator "
      english $ showOperator operator
      english ". One subtree on the left and the other subtree on the right, and once the other way round."
      german "Stellen Sie sich vor, die beiden angezeigten Bäume/Formeln würden unterhalb eines Wurzelknotens mit Operator "
      german $ showOperator operator
      german " gehängt. Einmal der eine Teilbaum links und der andere Teilbaum rechts, und einmal genau andersherum."

    when (isJust leftTreeImage) $ image $=<< liftIO $ cacheTree (fromJust leftTreeImage) path
    when (isNothing leftTreeImage) $ paragraph $ code $ display leftTree

    when (isJust rightTreeImage) $ image $=<< liftIO $ cacheTree (fromJust rightTreeImage) path
    when (isNothing rightTreeImage) $ paragraph $ code $ display rightTree

    instruct $ do
      english "Enter the formula represented by each of the two resulting trees."
      german "Geben Sie für die beiden entstehenden Bäume jeweils die dadurch repräsentierte Formel ein."

    instruct $ do
      english "(You are allowed to add arbitrarily many additional pairs of brackets.)"
      german "(Dabei dürfen Sie beliebig viele zusätzliche Klammerpaare hinzufügen.)"

    when addExtraHintsOnSemanticEquivalence $ instruct $ do
        english "Remarks: The exact formulas of the syntax trees must be specified. "
        english "Other formulas that are semantically equivalent to these formulas are incorrect solutions! "
        english "You are also not allowed to use associativity in this task in order to save brackets."
        german "Hinweise: Es müssen die exakten Formeln der Syntaxbäume angegeben werden. "
        german "Andere, selbst zu dieser Formel semantisch äquivalente Formeln sind keine korrekte Lösung! "
        german "Auch dürfen Sie bei dieser Aufgabe nicht Assoziativität verwenden, um Klammern einzusparen."

    keyHeading
    fullKey

    paragraph $ indent $ do
      translate $ do
        english "A valid solution could look like this: "
        german "Ein Lösungsversuch könnte beispielsweise so aussehen: "
      translatedCode $ flip localise $ translations $ do
        english "((A or not B) and C, C and (A or not B))"
        german "((A oder nicht B) und C, C und (A oder nicht B))"
      pure ()

    extra addText
    pure ()



verifyInst :: OutputMonad m => ComposeFormulaInst -> LangM m
verifyInst _ = pure ()



verifyConfig :: OutputMonad m => SynTreeConfig -> LangM m
verifyConfig = checkSynTreeConfig



start :: TreeFormulaAnswer
start = TreeFormulaAnswer Nothing



partialGrade :: OutputMonad m => ComposeFormulaInst -> (TreeFormulaAnswer, TreeFormulaAnswer) -> LangM m
partialGrade ComposeFormulaInst{..} (l,r)
  | isNothing (maybeTree l) || isNothing (maybeTree r) =
    reject $ do
      english "Your submission misses at least one formula."
      german "Sie haben mindestens eine Formel nicht angegeben."
  | not (all containsOperator [l,r]) =
    reject $ do
      english "At least one of your formulas does not contain the given operator."
      german "Mindestens eine der Formeln beinhaltet nicht den vorgegebenen Operator."
  | any (`notElem` correctLits) literals =
    reject $ do
      english "Your solution contains unknown literals."
      german "Ihre Abgabe beinhaltet unbekannte Literale."
  | any (`notElem` literals) correctLits =
    reject $ do
      english "Your solution does not contain all literals present in the original syntax trees."
      german "Ihre Abgabe beinhaltet nicht alle Literale aus den ursprünglichen Syntaxbäumen."
  | usedOperators > correctOperators =
    reject $ do
      english "Your solutions contains too many unique operators."
      german $ "Ihre Abgabe beinhaltet zu viele unterschiedliche Operatoren." ++ show usedOperators

  | otherwise = pure()
    where
      containsOperator = (operator `elem`) . collectUniqueBinOpsInSynTree . fromJust . maybeTree
      correctLits = nubOrd $ collectLeaves leftTree ++ collectLeaves rightTree
      literals = nubOrd $ collectLeaves (pForm l) ++ collectLeaves (pForm r)
      pForm = fromJust . maybeTree
      usedOperators = length $ nubOrd $
        collectUniqueBinOpsInSynTree (pForm l) ++
          collectUniqueBinOpsInSynTree (pForm r) ++ [operator]
      correctOperators = length $ nubOrd $
        collectUniqueBinOpsInSynTree leftTree ++
          collectUniqueBinOpsInSynTree rightTree ++ [operator]


completeGrade :: (OutputMonad m, MonadIO m) =>
  FilePath -> ComposeFormulaInst -> (TreeFormulaAnswer, TreeFormulaAnswer) -> LangM m
completeGrade path ComposeFormulaInst{..} (l,r)
  | not ((lSol,rSol) == (lrTree, rlTree) || (lSol,rSol) == (rlTree,lrTree)) = refuse $ do
    instruct $ do
      english "Your solution is not correct. The syntax tree for your entered formulas look like this:"
      german "Ihre Abgabe ist nicht die korrekte Lösung. Die Syntaxbäume zu Ihren eingegebenen Formeln sehen so aus:"

    image $=<< liftIO $ cacheTree (transferToPicture lSol) path
    image $=<< liftIO $ cacheTree (transferToPicture rSol) path

    when (lSol == rSol && (rSol == lrTree || rSol == rlTree)) $
      instruct $ do
        english "The two formulas entered only cover one of the two compositions."
        german "Die beiden eingegebenen Formeln decken nur eine der zwei Kompositionen ab."

    when (addExtraHintsOnSemanticEquivalence && (isSemanticEqual lSol lrTree || isSemanticEqual rSol lrTree)) $
        instruct $ do
          english "At least one syntax tree is semantically equivalent to the desired solution, but not identical."
          german "Mindestens ein Syntaxbaum ist semantisch äquivalent zum gewünschten Ergebnis, aber nicht identisch."

    when showSolution $
      example (show (display lrTree)) $ do
        english "A possible solution for this task is:"
        german "Eine mögliche Lösung für die Aufgabe ist:"

    pure ()
  | otherwise = pure ()
    where
      lSol = fromJust $ maybeTree l
      rSol = fromJust $ maybeTree r
      lrTree = Binary operator leftTree rightTree
      rlTree = Binary operator rightTree leftTree
