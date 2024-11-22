{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module LogicTasks.Semantics.Resolve where


import Data.Set (fromList, member, toList, unions)
import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  english,
  german,
  translate,
  translations,
  localise,
  yesNo,
  recoverFrom,
  )
import Data.List (sort)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Test.QuickCheck (Gen)

import Config (ResolutionConfig(..), ResolutionInst(..), BaseConfig(..))
import Formula.Util (isEmptyClause, mkCnf, sat)
import Formula.Resolution (applySteps, genRes, resolvableWith, resolve)
import Formula.Types (Clause, ResStep(..), literals)
import LogicTasks.Helpers (example, extra, keyHeading, negationKey, orKey)
import Util (checkBaseConf, prevent, preventWithHint)
import Control.Monad (unless, when)
import Control.Applicative (Alternative)
import Data.Foldable.Extra (notNull)
import Text.PrettyPrint.Leijen.Text (Pretty(pretty))
import Formula.Parsing.Delayed (Delayed, withDelayed, complainAboutWrongNotation, withDelayedSucceeding)
import Formula.Parsing (resStepsParser, clauseSetParser, clauseFormulaParser)
import Formula.Helpers (showCnfAsSet)




fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a


snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b


third3 :: (a,b,c) -> c
third3 (_,_,c) = c



genResInst :: ResolutionConfig -> Gen ResolutionInst
genResInst ResolutionConfig{ baseConf = BaseConfig{..}, ..} = do
  (clauses,solution) <- inst
  pure $ ResolutionInst {
    clauses = clauses,
    solution,
    printFeedbackImmediately = printFeedbackImmediately,
    usesSetNotation = useSetNotation,
    showSolution = printSolution,
    addText = extraText,
    unicodeAllowed = offerUnicodeInput
  }
  where
    inst = genRes (minClauseLength, maxClauseLength) minSteps usedLiterals



description :: OutputCapable m => Bool -> ResolutionInst -> LangM m
description oneInput ResolutionInst{..} = do
  paragraph $ do
    translate $ do
      german "Betrachten Sie die folgende Formel in KNF:"
      english "Consider the following formula in cnf:"
    indent $ code $ show' clauses
    pure ()
  paragraph $ translate $ do
    german "Führen Sie das Resolutionsverfahren an dieser Formel durch, um die leere Klausel abzuleiten."
    english "Use the resolution technique on this formula to derive the empty clause."

  paragraph $ translate $ if oneInput
    then do
      german "Geben Sie die Lösung als eine Liste von Tripeln an, wobei diese folgendermaßen aufgebaut sind: (Erste Klausel, Zweite Klausel, Resolvente)"
      english "Provide the solution as a list of triples with this structure: (first clause, second clause, resolvent)."
    else do
      german "Geben Sie die Lösung als eine Auflistung von Schritten an. "
      german "Füllen Sie für jeden Schritt die zugehörigen drei Eingabefelder mit den zwei verwendeten Klauseln sowie der daraus entstehenden Resolventen. "
      german "Schritte können nicht partiell ausgefüllt werden. Wenn Sie einen Schritt hinzufügen, MUSS dieser vollständig sein. "
      german "Bei Nichtbeachtung wird Ihre Abgabe aus Syntaxgründen abgelehnt. "
      german "Es ist aber erlaubt Schritte komplett auszulassen, z. B. wenn Sie weniger Schritte benötigen, als im Eingabeformular angegeben."
      english "Provide the solution as a sequence of steps. "
      english "Fill in the three input fields for each step with the two used clauses and the resulting resolvent. "
      english "Steps can not be partially filled in. Each added step MUST be complete. "
      english "Submissions containing partially filled in steps will be rejected as syntactically wrong. "
      english "You are allowed to entirely skip steps, e.g. if your solution has fewer steps overall than the amount of step inputs."
  keyHeading
  negationKey unicodeAllowed
  unless usesSetNotation (orKey unicodeAllowed)

  when usesSetNotation $ paragraph $ indent $ do
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

  paragraph $ translate $ if oneInput
    then do
      german "Neu resolvierte Klauseln können mit einer Nummer versehen werden, indem Sie '= NUMMER' an diese anfügen."
      english "Newly resolved clauses can be associated with a number by attaching '= NUMBER' behind them."
    else do
      german "Neu resolvierte Klauseln erhalten automatisch die Nummer rechts neben ihrem Eingabefeld."
      english "Newly resolved clauses are automatically assigned the number directly right of their input field."
  when usesSetNotation $ paragraph $ indent $ do
    translate $ do
      german "Nutzen Sie zur Angabe der Klauseln die Mengennotation! Ein Lösungsversuch könnte beispielsweise so aussehen: "
      english "Specify the clauses using set notation! A solution attempt could look like this: "
    translatedCode $ flip localise $ translations setExample
    pure ()

  unless usesSetNotation $ paragraph $ indent $ do
    translate $ do
      german "Nutzen Sie zur Angabe der Klauseln eine Formel! Ein Lösungsversuch könnte beispielsweise so aussehen: "
      english "Specify the clauses using a formula! A solution attempt could look like this: "
    translatedCode $ flip localise $ translations exampleCode
    pure ()

  extra addText
  pure ()
    where
      show' = if usesSetNotation
        then showCnfAsSet . mkCnf
        else show . mkCnf

      setExample
        | unicodeAllowed && oneInput = do
          english "[(1, 2, {A}), (3, 4, {¬A, ¬B} = 6), (5, 6, {not A}), ({A}, {not A}, {})]"
          german "[(1, 2, {A}), (3, 4, {¬A, ¬B} = 6), (5, 6, {nicht A}), ({A}, {nicht A}, {})]"
        | not unicodeAllowed && oneInput = do
          english "[(1, 2, {A}), (3, 4, {-A, -B} = 6), (5, 6, {not A}), ({A}, {not A}, {})]"
          german "[(1, 2, {A}), (3, 4, {-A, -B} = 6), (5, 6, {nicht A}), ({A}, {nicht A}, {})]"
        | unicodeAllowed && not oneInput = do
          english $ unlines
            [ "Step 1: First Clause:   1, Second Clause:       2, Resolvent: {A}       = 6"
            , "Step 2: First Clause:   3, Second Clause:       4, Resolvent: {¬A ∨ ¬B} = 7"
            , "Step 3: First Clause:   5, Second Clause:       7, Resolvent: {not A}   = 8"
            , "Step 4: First Clause: {A}, Second Clause: {not A}, Resolvent: {}        = 9"
            ]
          german $ unlines
            [ "Schritt 1: Erste Klausel:   1, Zweite Klausel:       2, Resolvente: {A}       = 6" -- no-spell-check
            , "Schritt 2: Erste Klausel:   3, Zweite Klausel:       4, Resolvente: {¬A ∨ ¬B} = 7" -- no-spell-check
            , "Schritt 3: Erste Klausel:   5, Zweite Klausel:       7, Resolvente: {not A}   = 8" -- no-spell-check
            , "Schritt 4: Erste Klausel: {A}, Zweite Klausel: {not A}, Resolvente: {}        = 9" -- no-spell-check
            ]
        | otherwise = do
          english $ unlines
            [ "Step 1: First Clause:   1, Second Clause:       2, Resolvent: {A}        = 6"
            , "Step 2: First Clause:   3, Second Clause:       4, Resolvent: {-A or -B} = 7"
            , "Step 3: First Clause:   5, Second Clause:       7, Resolvent: {not A}    = 8"
            , "Step 4: First Clause: {A}, Second Clause: {not A}, Resolvent: {}         = 9"
            ]
          german $ unlines
            [ "Schritt 1: Erste Klausel:   1, Zweite Klausel:         2, Resolvente: {A}          = 6" -- no-spell-check
            , "Schritt 2: Erste Klausel:   3, Zweite Klausel:         4, Resolvente: {-A oder -B} = 7" -- no-spell-check
            , "Schritt 3: Erste Klausel:   5, Zweite Klausel:         7, Resolvente: {nicht A}    = 8" -- no-spell-check
            , "Schritt 4: Erste Klausel: {A}, Zweite Klausel: {nicht A}, Resolvente: {}           = 9" -- no-spell-check
            ]
      exampleCode
        | unicodeAllowed && oneInput = do
          english "[(1, 2, A), (3, 4, ¬A ∨ ¬B = 6), (5, 6, not A), (A, not A, {})]"
          german "[(1, 2, A), (3, 4, ¬A ∨ ¬B = 6), (5, 6, nicht A), (A, nicht A, {})]"
        | not unicodeAllowed && oneInput = do
          english "[(1, 2, A), (3, 4, -A or -B = 6), (5, 6, not A), (A, not A, {})]"
          german "[(1, 2, A), (3, 4, -A oder -B = 6), (5, 6, nicht A), (A, nicht A, {})]"
        | unicodeAllowed && not oneInput = do
          english $ unlines
            [ "Step 1: First Clause: 1, Second Clause:     2, Resolvent: A       = 6"
            , "Step 2: First Clause: 3, Second Clause:     4, Resolvent: ¬A ∨ ¬B = 7"
            , "Step 3: First Clause: 5, Second Clause:     7, Resolvent: not A   = 8"
            , "Step 4: First Clause: A, Second Clause: not A, Resolvent: {}      = 9"
            ]
          german $ unlines
            [ "Schritt 1: Erste Klausel: 1, Zweite Klausel:     2, Resolvente: A       = 6" -- no-spell-check
            , "Schritt 2: Erste Klausel: 3, Zweite Klausel:     4, Resolvente: ¬A ∨ ¬B = 7" -- no-spell-check
            , "Schritt 3: Erste Klausel: 5, Zweite Klausel:     7, Resolvente: not A   = 8" -- no-spell-check
            , "Schritt 4: Erste Klausel: A, Zweite Klausel: not A, Resolvente: {}      = 9" -- no-spell-check
            ]
        | otherwise = do
          english $ unlines
            [ "Step 1: First Clause: 1, Second Clause:     2, Resolvent: A        = 6"
            , "Step 2: First Clause: 3, Second Clause:     4, Resolvent: -A or -B = 7"
            , "Step 3: First Clause: 5, Second Clause:     7, Resolvent: not A    = 8"
            , "Step 4: First Clause: A, Second Clause: not A, Resolvent: {}       = 9"
            ]
          german $ unlines
            [ "Schritt 1: Erste Klausel: 1, Zweite Klausel:       2, Resolvente: A          = 6" -- no-spell-check
            , "Schritt 2: Erste Klausel: 3, Zweite Klausel:       4, Resolvente: -A oder -B = 7" -- no-spell-check
            , "Schritt 3: Erste Klausel: 5, Zweite Klausel:       7, Resolvente: nicht A    = 8" -- no-spell-check
            , "Schritt 4: Erste Klausel: A, Zweite Klausel: nicht A, Resolvente: {}         = 9" -- no-spell-check
            ]


verifyStatic :: OutputCapable m => ResolutionInst -> LangM m
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



verifyQuiz :: OutputCapable m => ResolutionConfig -> LangM m
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

gradeSteps :: OutputCapable m => [(Clause,Clause,Clause)] -> Bool -> LangM m
gradeSteps steps appliedIsNothing = do
    preventWithHint (notNull noResolveSteps)
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

    prevent checkEmptyClause $
      translate $ do
        german "Letzter Schritt leitet die leere Klausel ab?"
        english "The last step derives the empty clause?"

    preventWithHint appliedIsNothing
      (translate $ do
        german "Alle Schritte nutzen vorhandene oder zuvor abgeleitete Klauseln?"
        english "All steps utilize existing or previously derived clauses?"
      )
      (paragraph $ do
        translate $ do
          german "Mindestens ein Schritt beinhaltet eine Klausel, die weder in der Formel vorhanden ist, noch zuvor abgeleitet wurde."
          english "At least one step contains a clause that is neither present in the formula nor was previously derived."
      )

    pure ()
    where
      noResolveSteps = filter (\(c1,c2,r) -> maybe True (\x ->
            fromJust (resolve c1 c2 x) /= r) (resolvableWith c1 c2)) steps
      checkEmptyClause = null steps || not (isEmptyClause $ third3 $ last steps)

partialGrade :: OutputCapable m => ResolutionInst -> Delayed [ResStep] -> LangM m
partialGrade inst = (partialGrade' inst `withDelayed` resStepsParser clauseParser) (const complainAboutWrongNotation)
  where clauseParser | usesSetNotation inst = clauseSetParser
                     | otherwise      = clauseFormulaParser

partialGrade' :: OutputCapable m => ResolutionInst -> [ResStep] -> LangM m
partialGrade' ResolutionInst{..} sol = do
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
    stepsGraded

  pure ()
  where
    checkMapping = correctMapping (zip [1..] sol) $ baseMapping clauses
    steps =  replaceAll sol $ baseMapping clauses
    availLits = unions (map (fromList . literals) clauses)
    stepLits (c1,c2,r) = toList $ unions $ map (fromList . literals) [c1,c2,r]
    wrongLitsSteps = filter (not . all (`member` availLits) . stepLits) steps
    applied = applySteps clauses steps
    stepsGraded = gradeSteps steps (isNothing applied)

completeGrade :: (OutputCapable m, Alternative m) => ResolutionInst -> Delayed [ResStep] -> LangM m
completeGrade inst = completeGrade' inst `withDelayedSucceeding` resStepsParser clauseParser
  where clauseParser | usesSetNotation inst = clauseSetParser
                     | otherwise      = clauseFormulaParser

completeGrade' :: (OutputCapable m, Alternative m) => ResolutionInst -> [ResStep] -> LangM m
completeGrade' ResolutionInst{..} sol = (if isCorrect then id else refuse) $ do
    unless printFeedbackImmediately $ do
      recoverFrom stepsGraded

    yesNo isCorrect $ translate $ do
      german "Lösung ist korrekt?"
      english "Solution is correct?"

    when (showSolution && not isCorrect) $
      example (show (pretty solution)) $ do
        english "A possible solution for this task is:"
        german "Eine mögliche Lösung für die Aufgabe ist:"

    pure ()
  where
    steps = replaceAll sol $ baseMapping clauses
    applied = applySteps clauses steps
    stepsGraded = gradeSteps steps (isNothing applied)
    isCorrect = any isEmptyClause (fromMaybe [] applied)

baseMapping :: [Clause] -> [(Int,Clause)]
baseMapping xs = zip [1..] $ sort xs



correctMapping :: OutputCapable m => [(Int,ResStep)] -> [(Int,Clause)] -> LangM m
correctMapping [] _ = pure()
correctMapping ((j, Res (c1,c2,(c3,i))): rest) mapping = do
  prevent checkIndices $
    translate $ do
      german $ show j ++ ". Schritt verwendet nur existierende Indizes?"
      english $ "Step " ++ show j ++ " uses only valid indices?"

  prevent (alreadyUsed i) $
    translate $ do
      german $ show j ++ ". Schritt vergibt keinen Index wiederholt?"
      english $ "Step " ++ show j ++ " does not assign an index repeatedly?"

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
