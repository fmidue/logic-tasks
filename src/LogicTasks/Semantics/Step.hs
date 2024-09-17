{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}

module LogicTasks.Semantics.Step where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  english,
  german,
  translate, localise, translations,
  )
import Data.Maybe (fromJust, isNothing)
import Data.List (delete)
import Data.Set (difference, fromList, member, toList, union)
import Test.QuickCheck (Gen, elements)

import Config (StepAnswer(..), StepConfig(..), StepInst(..), BaseConfig(..))
import Formula.Util (isEmptyClause, mkClause)
import Formula.Types (Clause, Literal(..), genClause, literals, opposite)
import Formula.Resolution (resolvable, resolve)
import LogicTasks.Helpers (example, extra, keyHeading, negationKey, orKey)
import Util (checkBaseConf, prevent, preventWithHint, tryGen)
import Control.Monad (when, unless)
import Formula.Parsing.Delayed (Delayed, withDelayed, complainAboutWrongNotation, withDelayedSucceeding)
import Formula.Parsing (clauseFormulaParser, stepAnswerParser, clauseSetParser)
import Formula.Helpers (showClauseAsSet)




genStepInst :: StepConfig -> Gen StepInst
genStepInst StepConfig{ baseConf = BaseConfig{..}, ..} = do
    (clause2, resolveLit, lits1) <- genResStepClause minClauseLength maxClauseLength usedLiterals
    let
      litAddedClause1 = mkClause $ resolveLit : lits1
      litAddedClause2 = mkClause $ opposite resolveLit : literals clause2
      resolutionClause = mkClause $ lits1 ++ filter (`notElem` lits1) (literals clause2)
    pure $ StepInst {
      clause1 = litAddedClause1,
      clause2 = litAddedClause2,
      solution = (resolveLit, resolutionClause),
      usesSetNotation = useSetNotation,
      showSolution = printSolution,
      addText = extraText,
      unicodeAllowed = offerUnicodeInput
    }



description :: OutputCapable m => StepInst -> LangM m
description StepInst{..} = do
  paragraph $ do
    translate $ do
      german "Betrachten Sie die zwei folgenden Klauseln:"
      english "Consider the two following clauses:"
    indent $ code $ show' clause1
    indent $ code $ show' clause2
    pure ()
  paragraph $ translate $ do
    german "Resolvieren Sie die Klauseln und geben Sie die Resolvente an."
    english "Resolve the clauses and give the resulting resolvent."

  paragraph $ translate $ do
    german "Geben Sie das in dem Resolutionsschritt genutzte Literal (in positiver oder negativer Form) und das Ergebnis in der folgenden Tupelform an: (Literal, Resolvente)."
    english "Provide the literal (in positive or negative form) used for the step and the resolvent in the following tuple form: (literal, resolvent)."

  keyHeading
  negationKey unicodeAllowed
  unless usesSetNotation (orKey unicodeAllowed)

  when usesSetNotation $ paragraph $ indent $ do
    translate $ do
      german "Nicht-leere Klausel:"
      english "Non-empty clause:"
    code "{ ... }"
    pure ()

  when usesSetNotation $ paragraph $ indent $ do
    translate $ do
      german "Nutzen Sie zur Angabe der Resolvente die Mengennotation! Ein Lösungsversuch könnte beispielsweise so aussehen: "
      english "Specify the resolvent using set notation! A valid solution could look like this: "
    translatedCode $ flip localise $ translations $ do
      german "(A, {nicht B, C})"
      english "(A, {not B, C})"
    pure ()

  unless usesSetNotation $ paragraph $ indent $ do
    translate $ do
      german "Nutzen Sie zur Angabe der Resolvente eine Formel! Ein Lösungsversuch könnte beispielsweise so aussehen: "
      english "Specify the resolvent using a formula! A valid solution could look like this: "
    translatedCode $ flip localise $ translations exampleCode
    pure ()


  extra addText
  pure ()
    where
      show' clause = if usesSetNotation
        then showClauseAsSet clause
        else show clause
      exampleCode | unicodeAllowed = do
                      german "(A, ¬B ∨ C)"
                      english "(A, ¬B ∨ C)"
                  | otherwise      = do
                      german "(A, nicht B oder C)"
                      english "(A, not B or C)"


verifyStatic :: OutputCapable m => StepInst -> LangM m
verifyStatic StepInst{..}
    | any isEmptyClause [clause1, clause2] =
        refuse $ indent $ translate $ do
          german "Mindestens eine der Klauseln ist leer."
          english "At least one of the clauses is empty."

    | not $ resolvable clause1 clause2 =
        refuse $ indent $ translate $ do
          german "Die Klauseln sind nicht resolvierbar."
          english "The clauses are not resolvable."

    | otherwise = pure()



verifyQuiz :: OutputCapable m => StepConfig -> LangM m
verifyQuiz StepConfig{..} = checkBaseConf baseConf



start :: StepAnswer
start = StepAnswer Nothing

partialGrade :: OutputCapable m => StepInst -> Delayed StepAnswer -> LangM m
partialGrade inst = (partialGrade' inst `withDelayed` stepAnswerParser clauseParser) (const complainAboutWrongNotation)
  where clauseParser | usesSetNotation inst = clauseSetParser
                     | otherwise            = clauseFormulaParser

partialGrade' :: OutputCapable m => StepInst -> StepAnswer -> LangM m
partialGrade' StepInst{..} sol = do

  prevent (isNothing $ step sol) $
    translate $ do
      german "Lösung ist nicht leer?"
      english "The solution is not empty?"

  prevent (not (fst mSol `member` availLits)) $
    translate $ do
      german "Das gewählte Literal kommt in einer der Klauseln vor?"
      english "The chosen literal is contained in any of the clauses?"

  preventWithHint (not $ null extraLiterals)
    (translate $ do
      german "Resolvente besteht aus bekannten Literalen?"
      english "Resolvent contains only known literals?"
    )
    (paragraph $ do
      translate $ do
        german "In der Resolvente sind unbekannte Literale enthalten. Diese Literale sind falsch: "
        english "The resolvent contains unknown literals. These literals are incorrect:"
      itemizeM $ map (text . show) extraLiterals
      pure ()
    )
  pure ()
  where
     mSol = fromJust $ step sol
     availLits = fromList (literals clause1) `union` fromList (literals clause2)
     solLits = fromList $ literals $ snd mSol
     extraLiterals = toList (solLits `difference` availLits)

completeGrade :: OutputCapable m => StepInst -> Delayed StepAnswer -> LangM m
completeGrade inst = completeGrade' inst `withDelayedSucceeding` stepAnswerParser clauseParser
  where clauseParser | usesSetNotation inst = clauseSetParser
                     | otherwise      = clauseFormulaParser


completeGrade' :: OutputCapable m => StepInst -> StepAnswer -> LangM m
completeGrade' StepInst{..} sol =
    case resolve clause1 clause2 (fst mSol) of
        Nothing -> refuse $ indent $ do
          translate $ do
            german "Mit diesem Literal kann kein Schritt durchgeführt werden!"
            english "This literal cannot be used for a resolution step!"

          displaySolution

          pure ()

        Just solClause -> if solClause == snd mSol
          then pure()
          else refuse $ indent $ do
            translate $ do
              german "Resolvente ist nicht korrekt."
              english "Resolvent is not correct."

            displaySolution

            pure ()
  where
    mSol = fromJust $ step sol
    displaySolution = when showSolution $ example (show solution) $ do
          english "A possible solution for this task is:"
          german "Eine mögliche Lösung für die Aufgabe ist:"


genResStepClause :: Int -> Int -> [Char] -> Gen (Clause, Literal, [Literal])
genResStepClause minClauseLength maxClauseLength usedLiterals = do
    rChar <- elements usedLiterals
    resolveLit <- elements [Literal rChar, Not rChar]
    let
      restLits = delete rChar usedLiterals
    minLen1 <- elements [minClauseLength-1..maxClauseLength-1]
    minLen2 <- elements [minClauseLength-1..maxClauseLength-1]
    clause1 <- genClause (minLen1,maxClauseLength-1) restLits
    let
      lits1 = literals clause1
    clause2 <- tryGen (genClause (minLen2,maxClauseLength-1) restLits) 100
        (all (\lit -> opposite lit `notElem` lits1) .  literals)
    pure (clause2, resolveLit, lits1)
