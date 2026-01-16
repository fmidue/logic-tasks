
taskName: ResolutionFullPrefilled

=============================================

module Global where

import LogicTasks.Config                (ResolutionInst)
import Formula.Types (ResStep)


type Submission = [ResStep]
type TaskData = ResolutionInst


=============================================


module TaskSettings where

import LogicTasks.Config                (dResConf, ResolutionConfig(..))
import Control.OutputCapable.Blocks (LangM, OutputCapable)
import LogicTasks.Semantics.Resolve     (verifyQuiz)


prefillSelect :: (Bool, Bool, Bool)
prefillSelect = (True, False, True)

resConf :: ResolutionConfig
resConf = dResConf

validateSettings :: OutputCapable m => LangM m
validateSettings = verifyQuiz resConf


=============================================


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TaskData (getTask) where

import Control.Monad.Random (MonadRandom)
import Data.String.Interpolate (i)

import FlexTask.GenUtil (fromGen)
import FlexTask.YesodConfig (Rendered, Widget)
import LogicTasks.Config                (ResolutionInst(..), ResolutionConfig(..))
import LogicTasks.Forms (fullResolutionForm)
import LogicTasks.Semantics.Resolve (genResInst)
import LogicTasks.Semantics.Step (showClause)
import Formula.Types (ResStep(..))

import Global
import TaskSettings


getTask :: MonadRandom m => m (TaskData, String, Rendered Widget)
getTask = do
    resInst <- fromGen $ genResInst resConf
    pure (resInst, checkers, form resInst)

form :: ResolutionInst -> Rendered Widget
form resInst = fullResolutionForm
    (minSteps resConf)
    (clauses resInst)
    (showClause (usesSetNotation resInst))
    (prefill prefillSelect (usesSetNotation resInst) (solution resInst))

prefill :: (Bool, Bool, Bool) -> Bool -> [ResStep] -> [(Maybe String, Maybe String, Maybe String)]
prefill (fill1, fill2, fill3) useSetNotation =
  map (\(Res (c1, c2, (c3, _))) ->
          ( if fill1 then
              case c1 of
                Left clause -> Just (showClause useSetNotation clause)
                Right j     -> Just (show j)
            else Nothing
          , if fill2 then
              case c2 of
                Left clause -> Just (showClause useSetNotation clause)
                Right j     -> Just (show j)
            else Nothing
          , if fill3 then
              Just (showClause useSetNotation c3)
            else Nothing
          )
      )

checkers :: String
checkers = [i|
{-\# LANGUAGE ApplicativeDo \#-}

module Check (checkSyntax, checkSemantics) where

import Control.Applicative (Alternative)

import Control.OutputCapable.Blocks
import LogicTasks.Semantics.Resolve (partialGrade', completeGrade')

import Global


checkSyntax :: OutputCapable m => FilePath -> TaskData -> Submission -> LangM m
checkSyntax _ = partialGrade'

checkSemantics :: (OutputCapable m, Alternative m) => FilePath -> TaskData -> Submission -> Rated m
checkSemantics _ taskData submission = do
    completeGrade' taskData submission
    pure 1.0
|]


=============================================


{-# LANGUAGE ApplicativeDo #-}

module Description (description) where

import Control.OutputCapable.Blocks
import LogicTasks.Semantics.Resolve (descriptionMultipleFields)

import Global


description :: OutputCapable m => FilePath -> TaskData -> LangM m
description _ =
    descriptionMultipleFields
    (do german "Unvollständig ausgefüllte Resolutionsschritte werden nicht berücksichtigt."
        english "Incomplete resolution steps will not be taken into account.")


=============================================


module Parse (parseSubmission) where

import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec
  ( sepBy1
  , string
  )

import LogicTasks.Config                (ResolutionConfig(..))
import Control.OutputCapable.Blocks.Generic (($>>=))
import Control.OutputCapable.Blocks
import FlexTask.Generic.Parse
import Formula.Parsing (clauseFormulaParser, clauseSetParser, resStepParser)
import Formula.Parsing.Delayed
import ParsingHelpers (fully)

import Global
import TaskSettings


instance Parse [(Maybe String, Maybe String, Maybe String, Int)] where
  formParser =
      sepBy1
          (formParser @(Maybe String, Maybe String, Maybe String, Int))
          (string "\a\a")

tup :: (Maybe String, Maybe String, Maybe String, Int) -> String
tup (c1,c2,c3,num) = if Nothing `elem` [c1,c2,c3]
  then ""
  else  '(' : fromJust c1 ++ "," ++ fromJust c2 ++ "," ++ fromJust c3 ++ "=" ++ show num ++ ")"

parseSubmission ::
  (Monad m, OutputCapable (ReportT o m))
  => String
  -> LangM' (ReportT o m) Submission
parseSubmission input =
  parseWithOrReport (formParser @[(Maybe String, Maybe String, Maybe String,Int)]) reportWithFieldNumber input $>>= \steps ->
    traverse parseIt (filter (/= "") (map tup steps))
  where
    parseIt =
      parseWithOrReport
        (fully (resStepParser (if useSetNotation resConf then clauseSetParser else clauseFormulaParser)))
        (\_ _ -> complainAboutWrongNotation)
