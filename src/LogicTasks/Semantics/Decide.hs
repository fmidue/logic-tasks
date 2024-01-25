{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}

module LogicTasks.Semantics.Decide where


import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  OutputMonad,
  english,
  german,
  translate,
  )
import Data.List (nub)
import Test.QuickCheck (Gen)

import Config (BaseConfig(..), CnfConfig(..), DecideConfig(..), DecideInst(..))
import Formula.Util (isEmptyCnf, hasEmptyClause)
import Formula.Table (flipAt, readEntries)
import Formula.Types (atomics, availableLetter, genCnf, getTable, literals)
import Util (checkCnfConf, isOutside, preventWithHint, remove)
import LogicTasks.Helpers (example, extra)
import Control.Monad (when)




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



description :: OutputMonad m => DecideInst -> LangM m
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


verifyStatic :: OutputMonad m => DecideInst -> LangM m
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

    | otherwise = pure()



verifyQuiz :: OutputMonad m => DecideConfig -> LangM m
verifyQuiz DecideConfig{..}
    | isOutside 1 100 percentageOfChanged =
        refuse $ indent $ translate $ do
          english "The percentile of mistakes has to be set between 1 and 100."
          german "Der prozentuale Anteil an Fehlern muss zwischen 1 und 100 liegen."

    | otherwise = checkCnfConf cnfConf



start :: [Int]
start = []



partialGrade :: OutputMonad m =>  DecideInst -> [Int] -> LangM m
partialGrade DecideInst{..} sol = do
  preventWithHint (solLen > acLen)
    (translate $ do
      german "Lösung enthält nicht zu viele Indizes?"
      english "Solution does not contain too many indices?"
    )
    (translate $ do
      german $ "Lösung enthält zu viele Indizes. Es " ++ ger ++" entfernt werden."
      english $ "Solution contains too many indices. Please remove " ++ eng ++ " to proceed."
    )

  preventWithHint (acLen > solLen)
    (translate $ do
      german "Lösung enthält genügend Indizes?"
      english "Solution contains enough indices?"
    )
    (translate $ do
      german $ "Lösung enthält zu wenige Indizes. Es " ++ ger ++ " hinzugefügt werden."
      english $ "Solution does not contain enough indices. Please add " ++ eng ++ " to proceed."
    )
  pure ()
  where
    acLen = length $ nub changed
    solLen = length $ nub sol
    distance = abs (solLen - acLen)
    display = show distance
    (ger, eng) = if distance == 1
    then ( "muss " ++ display ++ " spezifischer Wert", display ++ " unique value") -- no-spell-check
    else ("müssen " ++ display ++ " spezifische Werte", display ++ " unique values") -- no-spell-check



completeGrade :: OutputMonad m => DecideInst -> [Int] -> LangM m
completeGrade DecideInst{..} sol = do
  preventWithHint (diff /= 0)
    (translate $ do
      german "Lösung ist korrekt?"
      english "Solution is correct?"
    )
    (do
      translate $ do
        german $ "Die Lösung beinhaltet " ++ display ++ " Fehler."
        english $ "Your solution contains " ++ display ++ " mistakes."
      when showSolution $ example (show changed) $ do
        english "A possible solution for this task is:"
        german "Eine mögliche Lösung für die Aufgabe ist:"
      pure ()
    )
  where
    nubSol = nub sol
    diff = length $ filter (`notElem` changed) nubSol
    display = show diff
