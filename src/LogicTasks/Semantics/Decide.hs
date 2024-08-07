{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}

module LogicTasks.Semantics.Decide where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  english,
  german,
  translate,
  )
import Data.List (nub)
import Test.QuickCheck (Gen)

import Config (BaseConfig(..), CnfConfig(..), DecideConfig(..), DecideInst(..))
import Formula.Util (isEmptyCnf, hasEmptyClause)
import Formula.Table (flipAt, readEntries)
import Formula.Types (atomics, availableLetter, genCnf, getTable, literals, Table)
import Util (checkCnfConf, isOutside, preventWithHint, remove, printWithHint)
import LogicTasks.Helpers (example, extra)
import Control.Monad (when, unless)




genDecideInst :: DecideConfig -> Gen DecideInst
genDecideInst DecideConfig{cnfConf = CnfConfig {baseConf = BaseConfig{..}, ..}, ..} = do
    cnf <- getCnf
    let
      tableLen = length $ readEntries $ getTable cnf
      mistakeCount = max (tableLen * percentageOfChanged `div` 100) 1
    mistakes <- remove (tableLen - mistakeCount) [1..tableLen]
    pure $ DecideInst cnf mistakes printSolution extraText
  where
    getCnf = genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals



description :: OutputCapable m => DecideInst -> LangM m
description DecideInst{..} = do
  paragraph $ do
    translate $ do
      english "Consider the following formula:"
      german "Betrachten Sie die folgende Formel:"
    indent $ code $ availableLetter (literals cnf) : " = " ++ show cnf
    pure ()
  paragraph $ do
    translate $ do
      english "Find all faulty entries in the last column of the following truth table."
      german "Finden Sie alle fehlerhaften Wahrheitswerte in der letzten Spalte der folgenden Wahrheitstafel."
    indent $ code $ show (flipAt (getTable cnf) changed)
    pure ()
  paragraph $ translate $ do
    english  "Provide the solution as a list of indices of the faulty rows. The row with 0 for all atomic formulas counts as row 1."
    german  "Geben Sie die Lösung als eine Liste der Indizes der fehlerhaften Zeilen an. Dabei zählt die Zeile mit 0 für alle atomaren Formeln als Zeile 1."

  paragraph $ indent $ do
    translate $ do
      english "A valid solution could look like this: "
      german "Ein Lösungsversuch könnte beispielsweise so aussehen: "
    code "[1,4,5]"
    pure ()
  extra addText
  pure ()


verifyStatic :: OutputCapable m => DecideInst -> LangM m
verifyStatic DecideInst{..}
    | isEmptyCnf cnf || hasEmptyClause cnf =
        refuse $ indent $ translate $ do
          english "Please give a non empty formula."
          german "Geben Sie bitte eine nicht-leere Formel an."



    | any (> 2^length (atomics cnf)) changed || any (<=0) changed =
        refuse $ indent $ translate $ do
          english "At least one of the given indices does not exist."
          german "Mindestens einer der angegebenen Indizes existiert nicht."



    | null changed =
        refuse $ indent $ translate $ do
          english "At least one mistake has to be specified."
          german "Es muss mindestens eine Änderung geben."

    | otherwise = pure ()



verifyQuiz :: OutputCapable m => DecideConfig -> LangM m
verifyQuiz DecideConfig{..}
    | isOutside 1 100 percentageOfChanged =
        refuse $ indent $ translate $ do
          english "The percentile of mistakes has to be set between 1 and 100."
          german "Der prozentuale Anteil an Fehlern muss zwischen 1 und 100 liegen."

    | otherwise = checkCnfConf cnfConf



start :: [Int]
start = []

preventIfSolutionExceedsTableSize :: OutputCapable m => Int -> Table -> LangM m
preventIfSolutionExceedsTableSize solLen table = preventWithHint (solLen > tableLen)
    (translate $ do
      german "Lösung überschreitet nicht Anzahl der Zeilen?"
      english "Solution does not exceed count of rows?"
    )
    (translate $ do
      german $ "Ihre Lösung enthält mehr Werte als es Zeilen gibt. " ++ gerHint
      english $ "Your solution contains more values than there are rows. " ++ engHint
    )
  where
    tableLen = length $ readEntries table
    diffToTable = abs (solLen - tableLen)
    (gerLong,engLong) = gerEng diffToTable
    (gerHint,engHint) = (
      "Es " ++ gerLong ++" entfernt werden.", {- german -}
      "Please remove at least " ++ engLong ++ " to proceed."
      )
    gerEng diff = if diff == 1
        then ("muss mindestens " ++ display ++ " Wert", display ++ " value") -- no-spell-check
        else ("müssen mindestens " ++ display ++ " Werte", display ++ " values") -- no-spell-check
      where
        display = show diff

partialGrade :: OutputCapable m =>  DecideInst -> [Int] -> LangM m
partialGrade DecideInst{..} sol = do
  preventWithHint (null sol)
    (translate $ do
      german "Lösung enthält Indizes?"
      english "Solution contains indices?"
    )
    (translate $ do
      german "Die Lösung muss mindestens einen Index enthalten."
      english "The solution must contain at least one index."
    )

  preventIfSolutionExceedsTableSize (length sol) (getTable cnf)

  pure ()


completeGrade :: OutputCapable m => DecideInst -> [Int] -> LangM m
completeGrade DecideInst{..} sol = (if incorrect then refuse else id) $ do
  printWithHint (solLen > acLen)
    (translate $ do
      german "Lösung enthält nicht zu viele unterschiedliche Indizes?"
      english "Solution does not contain too many unique indices?"
    )
    (translate $ do
      german "Lösung enthält zu viele unterschiedliche Indizes."
      english "Solution contains too many unique indices."
    )

  unless (solLen > acLen) $
    printWithHint (acLen > solLen)
      (translate $ do
        german "Lösung enthält genügend unterschiedliche Indizes?"
        english "Solution contains enough unique indices?"
      )
      (translate $ do
        german "Lösung enthält zu wenige unterschiedliche Indizes."
        english "Solution does not contain enough unique indices."
      )

  printWithHint (diff /= 0)
    (translate $ do
      german "Indizes in der Lösung sind korrekt?"
      english "Indices in the solution are correct?"
    )
    (translate $ do
      german $ "In der Menge der unterschiedlichen Indizes " ++ ger ++ " falsch."
      english $ "The set of unique indices contains " ++ eng
    )


  when (incorrect && showSolution) $ example (show changed) $ do
      english "A possible solution for this task is:"
      german "Eine mögliche Lösung für die Aufgabe ist:"

  pure ()
  where
    nubSol = nub sol
    diff = length $ filter (`notElem` changed) nubSol
    acLen = length $ nub changed
    solLen = length $ nub sol
    incorrect = solLen > acLen || acLen > solLen || diff /= 0
    (ger, eng) = if diff == 1
      then ("ist 1 Index", "1 wrong index")
      else ("sind " ++ show diff ++ " Indizes", show diff ++ " wrong indices") -- no-spell-check
