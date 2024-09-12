{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module LogicTasks.Semantics.Fill where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  english,
  german,
  translate,
  )
import Data.Maybe (fromMaybe)
import Test.QuickCheck(Gen, suchThat)

import Config ( FillConfig(..), FillInst(..), FormulaInst (..), FormulaConfig (..))
import Formula.Table (gapsAt, readEntries)
import Formula.Types (TruthValue, availableLetter, atomics, getTable, literals, truth)
import Util (isOutside, pairwiseCheck, preventWithHint, remove, withRatio, tryGen, checkTruthValueRangeAndFormulaConf)
import Control.Monad (when)
import LogicTasks.Helpers (example, extra)
import Data.Foldable.Extra (notNull)
import Trees.Generate (genSynTree)
import Trees.Formula ()
import LogicTasks.Util (genCnf', genDnf', displayFormula, usesAllAtoms, isEmptyFormula)


genFillInst :: FillConfig -> Gen FillInst
genFillInst FillConfig{..} = do
    let percentTrueEntries' = fromMaybe (0,100) percentTrueEntries

    formula <- case formulaConfig of
      (FormulaArbitrary syntaxTreeConfig) ->
        InstArbitrary <$> genSynTree syntaxTreeConfig `suchThat` \t -> withRatio percentTrueEntries' t
      (FormulaCnf cnfCfg) ->
        tryGen (InstCnf <$> genCnf' cnfCfg) 100 $ withRatio percentTrueEntries'
      (FormulaDnf cnfCfg) ->
        tryGen (InstDnf <$> genDnf' cnfCfg) 100 $ withRatio percentTrueEntries'

    let
      entries = readEntries $ getTable formula
      tableLen = length entries
      gapCount = max (tableLen * percentageOfGaps `div` 100) 1
    gaps <- remove (tableLen - gapCount) [1..tableLen]
    let missingValues = [ b | (i, Just b) <- zip ([1..] :: [Int]) entries, i `elem` gaps]
    pure $ FillInst {
      formula
    , missing = gaps
    , missingValues
    , showSolution = printSolution
    , addText = extraText
    }




description :: OutputCapable m => FillInst -> LangM m
description FillInst{..} = do
  paragraph $ do
    translate $ do
      german  "Betrachten Sie die folgende Formel:"
      english "Consider the following formula:"
    indent $ code $ availableLetter (literals formula) : " = " ++ displayFormula formula
    pure ()
  paragraph $ do
    translate $ do
      german "Füllen Sie in der zugehörigen Wahrheitstafel alle Lücken mit einem passenden Wahrheitswert (Wahr oder Falsch)."
      english "Fill all blanks in the corresponding truth table with truth values (True or False)."
    indent $ code $ show $ gapsAt (getTable formula) missing
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


verifyStatic :: OutputCapable m => FillInst -> LangM m
verifyStatic FillInst{..}
    | isEmptyFormula formula =
        refuse $ indent $ translate $ do
          german "Geben Sie bitte eine nicht-leere Formel an."
          english "Please give a non empty formula."

    | any (> 2^length (atomics formula)) missing || any (<=0) missing =
    refuse $ indent $ translate $ do
      english "At least one of the given indices does not exist."
      german "Mindestens einer der angegebenen Indizes existiert nicht."


    | null missing =
        refuse $ indent $ translate $ do
          german "Es muss mindestens eine Lücke geben."
          english "At least one blank has to be specified."

    | otherwise = pure()



verifyQuiz :: OutputCapable m => FillConfig -> LangM m
verifyQuiz FillConfig{..}
    | isOutside 1 100 percentageOfGaps =
        refuse $ indent $ translate$ do
          german "Der prozentuale Anteil an Lücken muss zwischen 1 und 100 liegen."
          english "The percentile of gaps has to be set between 1 and 100."

    | not $ usesAllAtoms formulaConfig =
        refuse $ indent $ translate $ do
          german "Bei dieser Aufgabe müssen alle verfügbaren Atome verwendet werden."
          english "All available atoms must be used for this task."

    | otherwise = checkTruthValueRangeAndFormulaConf range formulaConfig
  where
    range = fromMaybe (0,100) percentTrueEntries



start :: [TruthValue]
start = []

partialGrade :: OutputCapable m => FillInst -> [TruthValue] -> LangM m
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

completeGrade :: OutputCapable m => FillInst -> [TruthValue] -> LangM m
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
      when showSolution $ example (show missingValues) $ do
        english "The solution for this task is:"
        german "Die Lösung für die Aufgabe ist:"
      pure ()
    )

  pure ()
  where
    boolSol = map truth sol
    zippedShort = zip3 boolSol missingValues [1..]
    (_,diff) = pairwiseCheck zippedShort
    displayMistake = show $ length diff
