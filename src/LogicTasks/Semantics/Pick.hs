{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module LogicTasks.Semantics.Pick where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  english,
  german,
  translate,
  )

import Test.QuickCheck (Gen, suchThat, elements)

import Config (Number(..), PickConfig(..), PickInst(..))
import Formula.Util (isSemanticEqual)
import Formula.Types (availableLetter, getTable, literals)
import Formula.Printing (showIndexedList)
import LogicTasks.Helpers (example, extra)
import Control.Monad (when)
import Data.Maybe (fromJust, fromMaybe)
import Trees.Generate (genSynTree)
import Tasks.SynTree.Config (SynTreeConfig (..))
import Trees.Print (display)
import Trees.Formula ()
import Util (withRatio, checkTruthValueRangeAndSynTreeConf, vectorOfUniqueBy)


genPickInst :: PickConfig -> Gen PickInst
genPickInst PickConfig{..} = do
  trees <- vectorOfUniqueBy
    amountOfOptions
    isSemanticEqual
    (genSynTree syntaxTreeConfig `suchThat` withRatio (fromMaybe (0,100) percentTrueEntries))

  correct <- elements [1..amountOfOptions]

  pure $ PickInst {
    trees,
    correct,
    showSolution = printSolution,
    addText = extraText
  }




description :: OutputCapable m => PickInst -> LangM m
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


verifyStatic :: OutputCapable m => PickInst -> LangM m
verifyStatic PickInst{..}
    | null trees =
        refuse $ indent $ translate $ do
          german "Die Liste der Formeln ist leer."
          english "The list of formulas is empty."

    | length trees < correct || correct <= 0 =
        refuse $ indent $ translate $ do
          german "Der angegebene Index existiert nicht."
          english "The given index does not exist."

    | otherwise = pure()



verifyQuiz :: OutputCapable m => PickConfig -> LangM m
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

    | rangeH - rangeL < 30 =
        refuse $ indent $ translate $ do
          german "Die Beschränkung der Wahr-Einträge sollte eine Reichweite von 30 nicht unterschreiten."
          english "The given restriction on true entries should not fall below a range of 30."

    | otherwise = checkTruthValueRangeAndSynTreeConf range syntaxTreeConfig
  where
    range@(rangeL, rangeH) = fromMaybe (0,100) percentTrueEntries



start :: Number
start = Number Nothing

partialGrade :: OutputCapable m => PickInst -> Number -> LangM m
partialGrade _ (Number Nothing) = refuse $ indent $
        translate $ do
          german "Es wurde kein Index angegeben."
          english "You did not give an index."
partialGrade _ _ = pure ()

completeGrade :: OutputCapable m => PickInst -> Number -> LangM m
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
