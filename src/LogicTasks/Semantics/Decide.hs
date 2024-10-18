{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module LogicTasks.Semantics.Decide where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  english,
  german,
  translate,
  Rated,
  translations,
  ArticleToUse (DefiniteArticle),
  reRefuse,
  extendedMultipleChoice,
  MinimumThreshold (MinimumThreshold),
  Punishment (Punishment),
  TargetedCorrect (TargetedCorrect),
  multipleChoiceSyntax,
  )
import Data.List.Extra (nubSort, nubOrd)
import Test.QuickCheck (Gen, suchThat)

import Config (DecideConfig(..), DecideInst(..), FormulaConfig (..), FormulaInst (..))
import Formula.Table (flipAt, readEntries)
import Formula.Types (atomics, availableLetter, getTable, literals)
import Util (isOutside, remove, withRatio, checkTruthValueRangeAndFormulaConf)
import LogicTasks.Helpers (extra)
import Control.Monad (when)
import Trees.Generate (genSynTree)
import Trees.Formula ()
import Data.Maybe (fromMaybe)
import LogicTasks.Util (genCnf', genDnf', displayFormula, usesAllAtoms, isEmptyFormula)
import qualified Data.Map as Map (fromAscList)
import Control.Applicative (Alternative)
import GHC.Real ((%))



genDecideInst :: DecideConfig -> Gen DecideInst
genDecideInst DecideConfig{..} = do
    let percentTrueEntries' = fromMaybe (0, 100) percentTrueEntries

    formula <- case formulaConfig of
      (FormulaArbitrary syntaxTreeConfig) ->
        InstArbitrary <$> genSynTree syntaxTreeConfig  `suchThat` withRatio percentTrueEntries'
      (FormulaCnf cnfCfg) ->
        InstCnf <$> genCnf' cnfCfg `suchThat` withRatio percentTrueEntries'
      (FormulaDnf cnfCfg) ->
        InstDnf <$> genDnf' cnfCfg `suchThat` withRatio percentTrueEntries'

    let
      tableLen = length $ readEntries $ getTable formula
      mistakeCount = max (tableLen * percentageOfChanged `div` 100) 1
    mistakes <- remove (tableLen - mistakeCount) [1..tableLen]
    pure $ DecideInst {
      formula
    , changed = mistakes
    , showSolution = printSolution
    , addText = extraText
    }



description :: OutputCapable m => DecideInst -> LangM m
description DecideInst{..} = do
  paragraph $ do
    translate $ do
      english "Consider the following formula:"
      german "Betrachten Sie die folgende Formel:"
    indent $ code $ availableLetter (literals formula) : " = " ++ displayFormula formula
    pure ()
  paragraph $ do
    translate $ do
      english "Find all faulty entries in the last column of the following truth table."
      german "Finden Sie alle fehlerhaften Wahrheitswerte in der letzten Spalte der folgenden Wahrheitstafel."
    indent $ code $ show (flipAt (getTable formula) changed)
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
    | isEmptyFormula formula =
        refuse $ indent $ translate $ do
          english "Please give a non-trivial formula."
          german "Geben Sie bitte eine nicht-triviale Formel an."

    | any (> 2^length (atomics formula)) changed || any (<=0) changed =
        refuse $ indent $ translate $ do
          english "At least one of the given indices does not exist."
          german "Mindestens einer der angegebenen Indizes existiert nicht."



    | null changed =
        refuse $ indent $ translate $ do
          english "At least one mistake has to be specified."
          german "Es muss mindestens einen zu findenden Fehler geben."

    | otherwise = pure ()



verifyQuiz :: OutputCapable m => DecideConfig -> LangM m
verifyQuiz DecideConfig{..}
    | isOutside 1 100 percentageOfChanged =
        refuse $ indent $ translate $ do
          english "The percentile of mistakes has to be set between 1 and 100."
          german "Der prozentuale Anteil an Fehlern muss zwischen 1 und 100 liegen."

    | not $ usesAllAtoms formulaConfig =
        refuse $ indent $ translate $ do
          german "Bei dieser Aufgabe müssen alle verfügbaren Atome verwendet werden."
          english "All available atoms must be used for this task."

    | otherwise = checkTruthValueRangeAndFormulaConf range formulaConfig
  where
    range = fromMaybe (0,100) percentTrueEntries



start :: [Int]
start = []

partialGrade :: OutputCapable m =>  DecideInst -> [Int] -> LangM m
partialGrade DecideInst{..} sol = multipleChoiceSyntax False [1..tableLen] $ nubOrd sol
    where
      table = getTable formula
      tableLen = length $ readEntries table


completeGrade :: (OutputCapable m,Alternative m, Monad m) => DecideInst -> [Int] -> Rated m
completeGrade DecideInst{..} sol = reRefuse
  (extendedMultipleChoice
    (MinimumThreshold (1 % 2))
    (Punishment (1 % fromIntegral tableLen))
    (TargetedCorrect (length changed))
    DefiniteArticle
    what
    solutionDisplay
    solution
    submission)
  $ when (diff /= 0) $ translate $ do
    german $ "In der Menge der unterschiedlichen Indizes " ++ ger ++ " falsch."
    english $ "The set of unique indices contains " ++ eng
  where
    nubSol = nubSort sol
    diff = length $ filter (`notElem` changed) nubSol
    (ger, eng) = if diff == 1
      then ("ist 1 Index", "1 wrong index")
      else ("sind " ++ show diff ++ " Indizes", show diff ++ " wrong indices") -- no-spell-check
    what = translations $ do
      german "Indizes"
      english "indices"
    solutionDisplay | showSolution = Just $ show changed
                    | otherwise = Nothing
    tableLen = length $ readEntries $ getTable formula
    solution = Map.fromAscList $ map (,True) changed
    submission = Map.fromAscList $ map (,True) nubSol
