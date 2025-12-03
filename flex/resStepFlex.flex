
taskName: ResolutionStepReverse

=============================================

{-# LANGUAGE PackageImports #-}

module Global where

import "logic-tasks" Config (StepInst)
import Formula.Types (Clause)


type Submission = Clause
type TaskData = StepInst


=============================================


{-# LANGUAGE PackageImports #-}

module TaskSettings where

import "logic-tasks" Config (dBaseConf, StepConfig(..))
import Control.OutputCapable.Blocks (LangM, OutputCapable)


stepConf :: StepConfig
stepConf = StepConfig
    { baseConf = dBaseConf
    , useSetNotation = False
    , printSolution = True
    , extraText = Nothing
    , offerUnicodeInput = True
    }

validateSettings :: OutputCapable m => LangM m
validateSettings = pure ()


=============================================


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PackageImports #-}

module TaskData (getTask) where

import Control.Monad.Random    (MonadRandom)
import Data.String.Interpolate (i)

import FlexTask.Generic.Form
import FlexTask.GenUtil        (fromGen)
import FlexTask.YesodConfig    (Rendered, Widget)
import LogicTasks.Semantics.Step (genStepInst)

import Global
import TaskSettings


getTask :: MonadRandom m => m (TaskData, String, Rendered Widget)
getTask = do
    resInst <- fromGen $ genStepInst stepConf
    pure (resInst, checkers, form)

form :: Rendered Widget
form = formify (Nothing :: Maybe String) [[single ""]]

checkers :: String
checkers = [i|
{-\# LANGUAGE ApplicativeDo \#-}
{-\# LANGUAGE PackageImports \#-}

module Check (checkSyntax, checkSemantics) where

import Control.Monad (when)

import "logic-tasks" Config (StepInst(..))
import Control.OutputCapable.Blocks
import Formula.Resolution (resolve)
import LogicTasks.Semantics.Step (showClause)

import Global


checkSyntax :: OutputCapable m => FilePath -> TaskData -> Submission -> LangM m
checkSyntax _ _ _ = pure ()

checkSemantics :: OutputCapable m => FilePath -> TaskData -> Submission -> Rated m
checkSemantics _ taskData submittedClause =
    case resolve (clause1 taskData) submittedClause (fst (solution taskData)) of
        Nothing -> do
            refuse $ indent $ do
                translate $ do
                    german "Das Literal mit welchem der Schritt durchgeführt wird, fehlt in der Klausel!"
                    english "The literal used to perform the step is missing from the clause!"
                displaySolution
                pure ()
            pure 0.0
        Just solClause ->
            if solClause == snd (solution taskData)
                then pure 1.0
                else do
                    refuse $ indent $ translate $ do
                        german "Die Resolution ist nicht korrekt."
                        english "Die Resolution is not correct."
                    pure 0.0
  where
    displaySolution = when (showSolution taskData) $ do
        translate $ do
            german "Eine mögliche Lösung für die Aufgabe ist:"
            english "A possible solution for this task is:"
        code (showClause (usesSetNotation taskData) (clause2 taskData))
        pure ()
|]


=============================================



{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}

module Description (description) where

import Control.OutputCapable.Blocks
import "logic-tasks" Config (StepInst(..))
import LogicTasks.Semantics.Step (showClause)
import LogicTasks.Keys

import Global


description :: OutputCapable m => FilePath -> TaskData -> LangM m
description _ StepInst{..} = do
    paragraph $ do
        translate $ do
            german "Betrachten Sie diese Resolvente als das Ergebnis eines Resolutionsschritts:"
            english "Consider this resolvent as a result of a resolution step:"
        indent $ code $ show' $ snd solution
        translate $ do
            german "Eine der zur Resolution verwendeten Klauseln ist:"
            english "One of the clauses used for the resolution is:"
        indent $ code $ show' clause1
        pure ()
    paragraph $ indent $ translate $ do
        german $ "Geben Sie eine mögliche zweite Klausel an, " ++
            "so dass ein Resolutionsschritt mit dieser und der gegebenen Klausel die obige Resolvente erzeugt."
        english "Provide a feasible second clause such that resolving it with the given clause results in the above resolvent."

    paragraph $ indent $ do
      translate notationText
      translatedCode $ flip localise $ translations example
      pure ()

    keyHeading
    negationKey unicodeAllowed
    key

    pure ()

  where
    show' = showClause usesSetNotation

    setExample = do
        german $ if unicodeAllowed then "{¬B, C}" else "{nicht B, C}"
        english $ if unicodeAllowed then "{¬B, C}" else "{not B, C}"

    exampleCode = do
        german $ if unicodeAllowed then "¬B ∨ C" else "nicht B oder C"
        english $ if unicodeAllowed then "¬B ∨ C" else "not B or C"

    (notationText,key,example) = if usesSetNotation
      then (
        do german "Nutzen Sie zur Angabe der Klausel die Mengenschreibweise! Ein Lösungsversuch könnte beispielsweise so aussehen: "
           english "Specify the clause using set notation! A valid solution could look like this: "
      , paragraph $ indent $ do
          translate $ do
            german "Nicht-leere Klausel:"
            english "Non-empty clause:"
          code "{ ... }"
          pure ()
      , setExample
      )
      else (
        do german "Nutzen Sie zur Angabe der Klausel eine Formel! Ein Lösungsversuch könnte beispielsweise so aussehen: "
           english "Specify the clause using a formula! A valid solution could look like this: "
      , orKey unicodeAllowed
      , exampleCode
      )


=============================================


{-# LANGUAGE PackageImports #-}

module Parse (parseSubmission) where

import "logic-tasks" Config (StepConfig(..))
import Control.OutputCapable.Blocks
import FlexTask.Generic.Parse
import Formula.Parsing (clauseFormulaParser, clauseSetParser)
import Formula.Types (Clause(..))

import Global
import TaskSettings


instance Parse Clause where
  formParser
    = escaped clauseParser
    where
      clauseParser
          | useSetNotation stepConf = clauseSetParser
          | otherwise                = clauseFormulaParser

parseSubmission ::
  (Monad m, OutputCapable (ReportT o m))
  => String
  -> LangM' (ReportT o m) Submission
parseSubmission = parseWithOrReport formParser reportWithFieldNumber
