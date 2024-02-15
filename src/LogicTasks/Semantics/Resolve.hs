{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module LogicTasks.Semantics.Resolve where


import Data.Set (fromList, member, toList, unions)
import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  OutputMonad,
  english,
  german,
  translate,
  translations,
  localise,
  yesNo,
  )
import Data.List (sort)
import Data.Maybe (fromJust, fromMaybe)
import Test.QuickCheck (Gen)

import Config (ResolutionConfig(..), ResolutionInst(..), BaseConfig(..))
import Formula.Util (isEmptyClause, mkCnf, sat)
import Formula.Resolution (applySteps, genRes, resolvableWith, resolve, computeResSteps)
import Formula.Types (Clause, ResStep(..), literals)
import LogicTasks.Helpers (example, extra, keyHeading, negationKey)
import Util (checkBaseConf, prevent, preventWithHint, printWithHint)
import Control.Monad (unless, when)
import Data.Foldable.Extra (notNull)
import Text.PrettyPrint.Leijen.Text (Pretty(pretty))




fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a


snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b


third3 :: (a,b,c) -> c
third3 (_,_,c) = c



genResInst :: ResolutionConfig -> Gen ResolutionInst
genResInst ResolutionConfig{ baseConf = BaseConfig{..}, ..} = do
  clauses <- inst
  pure $ ResolutionInst {
    clauses = clauses,
    printFeedbackImmediately = printFeedbackImmediately,
    showSolution = printSolution,
    addText = extraText
  }
  where
    inst = genRes (minClauseLength, maxClauseLength) minSteps usedLiterals



description :: OutputMonad m => ResolutionInst -> LangM m
description ResolutionInst{..} = do
  paragraph $ do
    translate $ do
      german "Betrachten Sie die folgende Formel in KNF:"
      english "Consider the following formula in cnf:"
    indent $ code $ show $ mkCnf clauses
    pure ()
  paragraph $ translate $ do
    german "Führen Sie das Resolutionsverfahren an dieser Formel durch, um die leere Klausel abzuleiten."
    english "Use the resolution technique on this formula to derive the empty clause."

  paragraph $ translate $ do
    german "Geben Sie die Lösung als eine Liste von Tripeln an, wobei diese folgendermaßen aufgebaut sind: (Erste Klausel, Zweite Klausel, Resolvente)"
    english "Provide the solution as a list of triples with this structure: (first clause, second clause, resolvent)."

  keyHeading
  negationKey

  paragraph $ indent $ do
    translate $ do
      german "Nicht-leere Klausel:"
      english "Non-empty clause:"
    code "{ ... }"
    pure ()

  paragraph $ indent $ do
    translate $ do
      german "Leere Klausel:"
      english "Empty clause:"
    code "{ }"
    pure ()
  paragraph $ translate $ do
    german "Optional können Sie Klauseln auch durch Nummern ersetzen."
    english "You can optionally replace clauses with numbers."

  paragraph $ translate $ do
    german "Klauseln aus der Formel sind bereits ihrer Reihenfolge nach nummeriert. (erste Klausel = 1, zweite Klausel = 2, ...)."
    english "Clauses in the starting formula are already numbered by their order. first clause = 1, second clause = 2, ...)."

  paragraph $ translate $ do
    german "Neu resolvierte Klauseln können mit einer Nummer versehen werden, indem Sie '= NUMMER' an diese anfügen."
    english "Newly resolved clauses can be associated with a number by attaching '= NUMBER' behind them."

  paragraph $ indent $ do
    translate $ do
      german "Ein Lösungsversuch könnte beispielsweise so aussehen: "
      english "A valid solution could look like this: "
    translatedCode $ flip localise $ translations $ do
      english "[(1, 2, {A, not B} = 5), (4, 5, { })]"
      german "[(1, 2, {A, nicht B} = 5), (4, 5, { })]"
    pure ()
  extra addText
  pure ()


verifyStatic :: OutputMonad m => ResolutionInst -> LangM m
verifyStatic ResolutionInst{..}
    | any isEmptyClause clauses =
        refuse $ indent $ translate $ do
          german "Mindestens eine der Klauseln ist leer."
          english "At least one of the clauses is empty."

    | sat $ mkCnf clauses =
        refuse $ indent $ translate $ do
          german "Die Formel ist erfüllbar."
          english "This formula is satisfiable."

    | otherwise = pure()



verifyQuiz :: OutputMonad m => ResolutionConfig -> LangM m
verifyQuiz ResolutionConfig{..}
    | minSteps < 1 =
        refuse $ indent $ translate $ do
          german "Die Mindestschritte müssen größer als 0 sein."
          english "The minimal amount of steps must be greater than 0."

    | maxClauseLength baseConf == 1 && minSteps > 1 =
        refuse $ indent $ translate $ do
          german "Mit Klauseln der Länge 1 kann nicht mehr als ein Schritt durchgeführt werden."
          english "More than one step using only length 1 clauses is not possible."

    | minSteps > 2 * length (usedLiterals baseConf) =
        refuse $ indent $ translate $ do
          german "Diese minimale Schrittzahl kann mit den gegebenen Literalen nicht durchgeführt werden."
          english "This amount of steps is impossible with the given amount of literals."

    | otherwise = checkBaseConf baseConf



start :: [ResStep]
start = []

-- Returns (True, ...) when the steps are correct
gradeSteps :: OutputMonad m => [ResStep] -> [Clause] -> (Bool, LangM m)
gradeSteps sol clauses = (not incorrect, do
    printWithHint (notNull noResolveSteps)
        (translate $ do
          german "Alle Schritte sind gültig?"
          english "All steps are valid?"
        )
        (paragraph $ do
          translate $ do
            german "Mindestens ein Schritt ist kein gültiger Resolutionsschritt. "
            english "At least one step is not a valid resolution step. "
          itemizeM $ map (text . show) noResolveSteps
          pure ()
        )

    yesNo (not checkEmptyClause) $
      translate $ do
        german "Letzter Schritt leitet die leere Klausel ab?"
        english "The last step derives the empty clause?"

    pure ()
  )
    where
      noResolveSteps = filter (\(c1,c2,r) -> maybe True (\x ->
            fromJust (resolve c1 c2 x) /= r) (resolvableWith c1 c2)) steps
      steps = replaceAll sol $ baseMapping clauses
      checkEmptyClause = null steps || not (isEmptyClause $ third3 $ last steps)
      incorrect = notNull noResolveSteps || checkEmptyClause


partialGrade :: OutputMonad m => ResolutionInst -> [ResStep] -> LangM m
partialGrade ResolutionInst{..} sol = do
  checkMapping

  preventWithHint (not $ null wrongLitsSteps)
    (translate $ do
      german "Genutzte Literale kommen in Formel vor?"
      english "Used literals are present in formula?"
    )
    (paragraph $ do
      translate $ do
        german "Mindestens ein Schritt beinhaltet Literale, die in der Formel nicht vorkommen. "
        english "At least one step contains literals not found in the original formula. "
      itemizeM $ map (text . show) wrongLitsSteps
      pure ()
    )

  when printFeedbackImmediately $ do
    (if fst stepsGraded then id else refuse) $ snd stepsGraded

  pure ()
  where
    checkMapping = correctMapping sol $ baseMapping clauses
    steps =  replaceAll sol $ baseMapping clauses
    availLits = unions (map (fromList . literals) clauses)
    stepLits (c1,c2,r) = toList $ unions $ map (fromList . literals) [c1,c2,r]
    wrongLitsSteps = filter (not . all (`member` availLits) . stepLits) steps
    stepsGraded = gradeSteps sol clauses

completeGrade :: OutputMonad m => ResolutionInst -> [ResStep] -> LangM m
completeGrade ResolutionInst{..} sol = (if isCorrect then id else refuse) $ do
    unless printFeedbackImmediately $ do
      snd stepsGraded

    yesNo isCorrect $ translate $ do
      german "Lösung ist korrekt?"
      english "Solution is correct?"

    when (showSolution && not isCorrect) $
      example (show . pretty $ computeResSteps clauses) $ do
        english "A possible solution for this task is:"
        german "Eine mögliche Lösung für die Aufgabe ist:"

    pure ()
  where
    steps = replaceAll sol $ baseMapping clauses
    applied = applySteps clauses steps
    stepsGraded = gradeSteps sol clauses
    isCorrect = any isEmptyClause (fromMaybe [] applied) && (printFeedbackImmediately || fst stepsGraded)

baseMapping :: [Clause] -> [(Int,Clause)]
baseMapping xs = zip [1..] $ sort xs



correctMapping :: OutputMonad m => [ResStep] -> [(Int,Clause)] -> LangM m
correctMapping [] _ = pure()
correctMapping (Res (c1,c2,(c3,i)): rest) mapping = do
  prevent checkIndices $
    translate $ do
      german "Alle Schritte verwenden existierende Indizes?"
      english "All steps use valid indices?"

  prevent (alreadyUsed i) $
    translate $ do
      german "Kein Index wird mehrfach vergeben?"
      english "No index is in duplicate use?"

  correctMapping rest newMapping
  pure ()
  where
    newMapping = case i of Nothing      -> mapping
                           (Just index) -> (index,c3) : mapping

    unknown (Left _) = False
    unknown (Right n) = n `notElem` map fst mapping
    checkIndices = unknown c1 || unknown c2
    alreadyUsed Nothing = False
    alreadyUsed (Just n) = n `elem` map fst mapping



replaceAll :: [ResStep] -> [(Int,Clause)] -> [(Clause,Clause,Clause)]
replaceAll [] _ = []
replaceAll (Res (c1,c2,(c3,i)) : rest) mapping = (replaceNum c1, replaceNum c2, c3) : replaceAll rest newMapping
  where
    newMapping = case i of Nothing      -> mapping
                           (Just index) -> (index,c3) : mapping

    replaceNum (Left c) = c
    replaceNum (Right n) = case lookup n mapping of Nothing  -> error "no mapping"
                                                    (Just c) -> c
