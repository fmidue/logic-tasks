{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}

module LogicTasks.Semantics.Fill where


import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  OutputMonad,
  english,
  german,
  translate,
  )
import Data.Maybe (fromMaybe, fromJust)
import Test.QuickCheck(Gen)

import Config ( BaseConfig(..), CnfConfig(..), FillConfig(..), FillInst(..))
import Formula.Util (hasEmptyClause, isEmptyCnf)
import Formula.Table (gapsAt, readEntries)
import Formula.Types (TruthValue, availableLetter, atomics, genCnf, getTable, literals, truth)
import Util (checkTruthValueRange, isOutside, pairwiseCheck, preventWithHint, remove, tryGen, withRatio)
import Control.Monad (when)
import LogicTasks.Helpers (example, extra)
import Data.Foldable.Extra (notNull)




genFillInst :: FillConfig -> Gen FillInst
genFillInst FillConfig{ cnfConf = CnfConfig { baseConf = BaseConfig{..}, ..}, ..} = do
    cnf <- cnfInRange
    let
      tableLen = length $ readEntries $ getTable cnf
      gapCount = max (tableLen * percentageOfGaps `div` 100) 1
    gaps <- remove (tableLen - gapCount) [1..tableLen]
    pure $ FillInst cnf gaps printSolution extraText
  where
    getCnf = genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals
    cnfInRange = tryGen getCnf 100 $ withRatio $ fromMaybe (0,100) percentTrueEntries



description :: OutputMonad m => FillInst -> LangM m
description FillInst{..} = do
  paragraph $ do
    translate $ do
      german  "Betrachten Sie die folgende Formel:"
      english "Consider the following formula:"
    indent $ code $ availableLetter (literals cnf) : " = " ++ show cnf
    pure ()
  paragraph $ do
    translate $ do
      german "Füllen Sie in der zugehörigen Wahrheitstafel alle Lücken mit einem passenden Wahrheitswert (Wahr oder Falsch)."
      english "Fill all blanks in the corresponding truth table with truth values (True or False)."
    indent $ code $ show $ gapsAt (getTable cnf) missing
    pure ()
  paragraph $ translate $ do
    german "Geben Sie als Lösung eine Liste der fehlenden Wahrheitswerte an, wobei das erste Element der Liste der ersten Lücke von oben entspricht, das zweite Element der zweiten Lücke, etc."
    english "Provide the solution as a list of truth values. The first element of the list fills the first blank from the top, the second element fills the second blank, etc."

  paragraph $ translate $ do
    german "Die Eingabe der Werte kann binär (0 = falsch, 1 = wahr), ausgeschrieben (falsch, wahr) oder als Kurzform (f, w) erfolgen."
    english "Values can be submitted in binary form (0 = false, 1 = true), by entering the entire word (false, true) or by giving a shorthand (f, t)."

  paragraph $ indent $ do
    translate $ do
      german "Ein Lösungsversuch im Fall von vier Lücken könnte beispielsweise so aussehen:"
      english "A valid solution for four blanks could look like this:"
    code "[0,1,1,1]"
    pure ()

  extra addText
  pure ()


verifyStatic :: OutputMonad m => FillInst -> LangM m
verifyStatic FillInst{..}
    | isEmptyCnf cnf || hasEmptyClause cnf =
        refuse $ indent $ translate $ do
          german "Geben Sie bitte eine nicht-leere Formel an."
          english "Please give a non empty formula."


    | any (> 2^length (atomics cnf)) missing || any (<=0) missing =
    refuse $ indent $ translate $ do
      english "At least one of the given indices does not exist."
      german "Mindestens einer der angegebenen Indizes existiert nicht."


    | null missing =
        refuse $ indent $ translate $ do
          german "Es muss mindestens eine Lücke geben."
          english "At least one blank has to be specified."

    | otherwise = pure()



verifyQuiz :: OutputMonad m => FillConfig -> LangM m
verifyQuiz FillConfig{..}
    | isOutside 1 100 percentageOfGaps =
        refuse $ indent $ translate$ do
          german "Der prozentuale Anteil an Lücken muss zwischen 1 und 100 liegen."
          english "The percentile of gaps has to be set between 1 and 100."

    | otherwise = checkTruthValueRange (low,high) cnfConf
  where
    (low,high) = fromMaybe (0,100) percentTrueEntries



start :: [TruthValue]
start = []



partialGrade :: OutputMonad m => FillInst -> [TruthValue] -> LangM m
partialGrade FillInst{..} sol = do
  preventWithHint (solLen /= missingLen)
    (translate $ do
      german "Lösung hat korrekte Länge?"
      english "Solution has correct length?"
    )
    (translate $ do
      german $ "Die Lösung muss genau "  ++ show missingLen ++ " Lücken enthalten."
      english $ "The solution must contain exactly " ++ show missingLen ++ " gaps."
    )

  pure ()
    where
      boolSol = map truth sol
      solLen = length boolSol
      missingLen = length missing


completeGrade :: OutputMonad m => FillInst -> [TruthValue] -> LangM m
completeGrade FillInst{..} sol = do
  preventWithHint (notNull diff)
    (translate $ do
      german "Lösung ist korrekt?"
      english "Solution is correct?"
    )
    (do
      translate $ do
        german $ "Die Lösung beinhaltet " ++ displayMistake ++ " Fehler."
        english $ "Your solution contains " ++ displayMistake ++ " mistakes."
      when showSolution $ example (show correctShort) $ do
        english "The solution for this task is:"
        german "Die Lösung für die Aufgabe ist:"
      pure ()
    )

  pure ()
  where
    table = getTable cnf
    allEntries = map fromJust $ readEntries table
    correctShort = [allEntries !! i | i <- map (\x -> x-1) missing]
    boolSol = map truth sol
    zippedShort = zip3 boolSol correctShort [1..]
    (_,diff) = pairwiseCheck zippedShort
    displayMistake = show $ length diff
