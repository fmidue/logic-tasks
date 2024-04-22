{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module LogicTasks.Semantics.Pick where


import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  OutputMonad,
  english,
  german,
  translate,
  )

import Test.QuickCheck (Gen, vectorOf, suchThat, elements)

import Config (Number(..), PickConfig(..), PickInst(..))
import Formula.Util (isSemanticEqual)
import Formula.Types (availableLetter, getTable, literals)
import Formula.Printing (showIndexedList)
import LogicTasks.Helpers (example, extra)
import Control.Monad (when)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (nubBy)
import Trees.Generate (genSynTree)
import Tasks.SynTree.Config (SynTreeConfig (..))
import Trees.Print (display)
import Trees.Formula ()
import Util (withRatio, checkTruthValueRangeAndSynTreeConf)


genPickInst :: PickConfig -> Gen PickInst
genPickInst PickConfig{..} = do
  trees <- vectorOf amountOfOptions (genSynTree syntaxTreeConfig) `suchThat` \trees ->
    length (nubBy isSemanticEqual trees) == amountOfOptions &&
    all (withRatio (fromMaybe (0, 100) percentTrueEntries)) trees

  correct <- elements [1..amountOfOptions]

  pure $ PickInst {
    trees,
    correct,
    showSolution = printSolution,
    addText = extraText
  }




description :: OutputMonad m => PickInst -> LangM m
description PickInst{..} = do
    paragraph $ do
      translate $ do
        german "Betrachten Sie die folgende Formel:"
        english "Consider the following formula:"
      indent $ code $ availableLetter (literals sTable) : " = " ++ display sTable
      pure ()
    paragraph $ do
      translate $ do
        german "Welche der folgenden Wahrheitstafeln passt zu der Formel? Geben Sie die richtige Tafel durch ihre Nummer an."
        english "Which of these truth tables represents the formula? Specify the correct table by giving its number."
      indent $ code $ showIndexedList 120 5 $ map getTable trees
      pure ()
    paragraph $ indent $ do
      translate $ do
        german "Ein Lösungsversuch könnte beispielsweise so aussehen: "
        english "A valid solution could look like this: "
      code "1"
      pure ()
    extra addText
    pure ()
  where
    sTable = trees !! (correct - 1)


verifyStatic :: OutputMonad m => PickInst -> LangM m
verifyStatic PickInst{..}
    | null trees =
        refuse $ indent $ translate $ do
          german "Die Liste der Formeln ist leer."
          english "The list of formulas is empty."

    | length trees < correct || correct <= 0 =
        refuse $ indent $ translate $ do
          german "Der angegebene Index existiert nicht."
          english "The given index does not exist."

    | otherwise = pure ()



verifyQuiz :: OutputMonad m => PickConfig -> LangM m
verifyQuiz PickConfig{..}

    | minAmountOfUniqueAtoms syntaxTreeConfig < 2 =
        refuse $ indent $ translate $ do
          german "Es muss mindestens zwei unterschiedliche Atome geben."
          english "At least two unique atoms are required."

    | amountOfOptions < 2 =
        refuse $ indent $ translate $ do
          german "Es muss mindestens zwei Optionen geben."
          english "At least two options need to be given."

    | amountOfOptions > 4*2^ length (availableAtoms syntaxTreeConfig) =
        refuse $ indent $ translate $ do
          german "Die Anzahl Optionen übersteigt die Anzahl möglicher, unterschiedlicher Formeln."
          english "The amount of options is higher than the amount of possible, distinct formulas."

    | minAmountOfUniqueAtoms syntaxTreeConfig /= fromIntegral (length (availableAtoms syntaxTreeConfig)) =
        refuse $ indent $ translate $ do
          german "Bei dieser Aufgabe müssen alle verfügbaren Atome verwendet werden."
          english "All available atoms must be used for this task."

    | otherwise = checkTruthValueRangeAndSynTreeConf range syntaxTreeConfig
  where
    range = fromMaybe (0,100) percentTrueEntries



start :: Number
start = Number Nothing

partialGrade :: OutputMonad m => PickInst -> Number -> LangM m
partialGrade _ (Number Nothing) = refuse $ indent $
        translate $ do
          german "Es wurde kein Index angegeben."
          english "You did not give an index."
partialGrade _ _ = pure ()

completeGrade :: OutputMonad m => PickInst -> Number -> LangM m
completeGrade PickInst{..} (Number index) =
    if fromJust index == correct
        then pure ()
        else refuse $ indent $ do
          translate $ do
            german "Der gewählte Index ist falsch."
            english "You submitted the wrong index."

          displaySolution

          pure ()
  where displaySolution = when showSolution $ example (show correct) $ do
          english "The solution for this task is:"
          german "Die Lösung für die Aufgabe ist:"
