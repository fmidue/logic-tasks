{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}

module LogicTasks.Semantics.Max where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  english,
  german,
  translate,
  translations,
  localise,
  )
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Test.QuickCheck (Gen)

import Config (BaseConfig(..), CnfConfig(..),  MaxInst(..), MinMaxConfig(..))
import Formula.Util (hasEmptyClause, isEmptyCnf, mkClause, mkCnf)
import Formula.Table (readEntries)
import Formula.Types (Cnf, Formula, Literal(..), amount, atomics, genCnf, getClauses, getTable)
import LogicTasks.Helpers (formulaKey, example, extra)
import Util (checkTruthValueRange, pairwiseCheck, prevent, preventWithHint, tryGen, withRatio)
import Control.Monad (when)
import Formula.Parsing.Delayed (Delayed, withDelayed)
import Formula.Parsing (Parse(..))



genMaxInst :: MinMaxConfig -> Gen MaxInst
genMaxInst MinMaxConfig {cnfConf = CnfConfig {baseConf = BaseConfig{..},..},..} = do
    cnf <- cnfInRange
    pure $ MaxInst cnf printSolution extraText
  where
    getCnf = genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals
    cnfInRange = tryGen getCnf 100 $ withRatio $ fromMaybe (0,100) percentTrueEntries



description :: OutputCapable m => MaxInst -> LangM m
description MaxInst{..} = do
  paragraph $ do
    translate $ do
      german "Betrachten Sie die folgende Wahrheitstafel:"
      english "Consider the following truth table:"
    indent $ code $ show $ getTable cnf
    pure ()
  paragraph $ translate $ do
    german "Geben Sie eine zu der Tafel passende Formel in konjunktiver Normalform an. Verwenden Sie dazu Max-Terme."
    english "Provide a formula in conjunctive normal form, that corresponds to the table. Use maxterms to do this."

  formulaKey

  paragraph $ indent $ do
    translate $ do
      let formulaStr = show $ mkCnf [mkClause [Literal 'A', Not 'B'], mkClause [Not 'C', Not 'D']]
      german $ unwords ["Ein Lösungsversuch für Formel", formulaStr, "könnte beispielsweise so aussehen: "]
      english $ unwords ["A solution attempt for the formula", formulaStr, "could look like this: "]
    translatedCode $ flip localise $ translations $ do
      german "(A oder nicht B) und (nicht C oder nicht D)"
      english "(A or not B) and (not C or not D)"
    pure ()
  extra addText
  pure ()


verifyStatic :: OutputCapable m => MaxInst -> LangM m
verifyStatic MaxInst{..}
    | isEmptyCnf cnf || hasEmptyClause cnf =
        refuse $ indent $ translate $ do
          german "Geben Sie bitte eine nicht-leere Formel an."
          english "Please give a non empty formula."

    | otherwise = pure()



verifyQuiz :: OutputCapable m => MinMaxConfig -> LangM m
verifyQuiz MinMaxConfig{..} = checkTruthValueRange (low,high) cnfConf
  where
    (low,high) = fromMaybe (0,100) percentTrueEntries



start :: Cnf
start = mkCnf [mkClause [Literal 'A']]



partialMinMax :: (OutputCapable m, Formula f) => [Literal] -> f -> f -> Bool -> Bool -> LangM m
partialMinMax correctLits correct solution allValidTerms isMaxTermTask = do
  preventWithHint (not $ null extraLiterals)
    (translate $ do
      german "Angegebene Literale kommen in Aufgabe vor?"
      english "Given literals are used in task?"
    )

    (paragraph $ do
      translate $ do
        german "Es sind unbekannte Literale enthalten. Diese Literale kommen in der korrekten Lösung nicht vor: "
        english "Your submission contains unknown literals. These do not appear in a correct solution: "
      itemizeM $ map (text . show) extraLiterals
      pure ()
    )

  preventWithHint (not $ null missing)
    (translate $ do
      german "Alle Literale kommen vor?"
      english "All literals are contained in solution?"
    )

    (paragraph $ do
      translate $ do
        german "Es fehlen Literale. Fügen Sie diese Literale der Abgabe hinzu: "
        english "Some literals are missing. Add these literals to your submission: "
      itemizeM $ map (text . show) missing
      pure ()
    )

  prevent allValidTerms $
    translate $ do
      german $ "Alle " ++ gSubElements ++ " sind " ++ gTerms ++ "?"
      english $ "All " ++ eSubElements ++ " are " ++ eTerms ++ "?"

  preventWithHint (solLen < corrLen)
    (translate $ do
      german $ "Genügend " ++ gTerms ++ " in Lösung?"
      english $ "Solution contains enough " ++ eTerms ++ "?"
    )

    (paragraph $ do
      translate $ do
        german $ "Die angegebene Formel enthält zu wenige " ++ gTerms ++ ". Fügen sie "
        english $ "The formula does not contain enough " ++ eTerms ++ ". Add "
      text diff
      translate $ do
        german " hinzu!"
        english "!"
      pure ()
    )

  preventWithHint (solLen > corrLen)
    (translate $ do
      german $ "Nicht zu viele " ++ gTerms ++ " in Lösung?"
      english $ "Not too many " ++ eTerms ++ " in solution?"
    )

    (paragraph $ do
      translate $ do
        german $ " Die angegebene Formel enthält zu viele " ++ gTerms ++ ". Entfernen sie "
        english $ "The formula contains too many " ++ eTerms ++ ". Remove "
      text $ diff ++ "!"
      pure ()
    )
  pure ()
 where
    solLits = atomics solution
    extraLiterals = solLits \\ correctLits
    missing = correctLits \\ solLits
    table = getTable correct
    corrLen = length $ filter (== Just False) (readEntries table)
    solLen = amount solution
    diff = show $ abs (solLen - corrLen)
    (gTerms, gSubElements, eTerms, eSubElements)= if isMaxTermTask
      then ("Maxterme", "Klauseln", "maxterms", "clauses") -- no-spell-check
      else ("Minterme", "Konjunktionen", "minterms", "conjunctions") -- no-spell-check

partialGrade :: OutputCapable m => MaxInst -> Delayed Cnf -> LangM m
partialGrade inst = partialGrade' inst `withDelayed` parser

partialGrade' :: OutputCapable m => MaxInst -> Cnf -> LangM m
partialGrade' MaxInst{..} sol = partialMinMax corLits cnf sol allMaxTerms True
  where
    corLits = atomics cnf
    allMaxTerms = not $ all (\c -> amount c == length corLits) $ getClauses sol



completeMinMax :: (OutputCapable m, Formula f, Show f) => Bool -> f -> f -> LangM m
completeMinMax showSolution correct solution =
    preventWithHint (not $ null diff)
      (translate $ do
         german "Lösung liefert korrekte Wahrheitstabelle?"
         english "Solution gives correct truth table?"
      )

      (do
        paragraph $ do
          translate $ do
            german "Es existieren falsche Einträge in den folgenden Tabellenzeilen: "
            english "The following rows are not correct: "
          itemizeM $ map (text . show) diff
          pure ()
        when showSolution $ example (show correct) $ do
          english "A possible solution for this task is:"
          german "Eine mögliche Lösung für die Aufgabe ist:"
        pure ()
      )
  where
    solTable = getTable solution
    (_,diff) = pairwiseCheck (zip3 (readEntries solTable) (readEntries $ getTable correct) [1..])

completeGrade :: OutputCapable m => MaxInst -> Delayed Cnf -> LangM m
completeGrade inst = completeGrade' inst `withDelayed` parser

completeGrade' :: OutputCapable m => MaxInst -> Cnf -> LangM m
completeGrade' MaxInst{..} = completeMinMax showSolution cnf
