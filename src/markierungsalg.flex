
taskName: MarkierungsAlgorithmus

=============================================

{-# LANGUAGE DeriveDataTypeable #-}

module Global where

import Data.Data (Data)

import LogicTasks.Formula (TruthValue)
import Trees.Types (SynTree, BinOp)
import FlexTask.Generic.Form (SingleChoiceSelection)


newtype CharAnswer = CharAnswer {unCharAnswer :: [Char]}
    deriving (Eq, Show)

data Submission = Submission
    { steps :: [Maybe CharAnswer]
    , output :: SingleChoiceSelection
    , model :: Maybe [(Char,TruthValue)]
    }

data Solution = Solution
    { correctSteps :: [(Int, [Char])]
    , correctOutput :: Bool
    , correctModel :: [(Char,Bool)]
    } deriving Data

data TaskData = TaskData
    { formula :: SynTree BinOp Char
    , solution :: Solution
    } deriving Data


=============================================


module TaskSettings where

import Data.List ((\\))

import Control.OutputCapable.Blocks
import Horn
import Trees.Types (SynTree(..), BinOp(..))


atomics :: [Char]
atomics = getAllAtomics spirit

stepFields :: Int
stepFields =  length atomics + extra

spirit :: [SynTree BinOp Char]
spirit = v3

extra :: Int
extra = 0

showSolution, printFeedbackImmediately :: Bool
showSolution = True
printFeedbackImmediately = True

minimumPoints :: Double
minimumPoints = 0.25

validateSettings :: OutputCapable m => LangM m
validateSettings
    | not (isHornFormulaI (foldr1 (Binary And) spirit)) = refuse $ indent $ text
        "Als spirit muss eine Liste von Hornklauseln angegeben werden."
    | extra > length atomics = refuse $ indent $ text $
        "Es können nur so viele extra Klauseln hinzugefügt werden, " ++
            "wie in spirit bereits enthalten sind."
    | (atomics \\ ['A' .. 'L']) /= [] = refuse $ indent $ text $
        "Es dürfen nur Buchstaben aus dem Intervall ['A' .. 'L'], " ++
            "für die atomaren Aussagen verwendet werden."
    | otherwise = pure ()


=============================================


{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TaskData (getTask) where

import Control.Monad.Random (MonadRandom)
import Data.String.Interpolate (i)
import Data.Text (pack)
import Yesod
    ( RenderMessage(..)
    , SomeMessage(..)
    , fieldSettingsLabel
    )

import FlexTask.Generic.Form
import FlexTask.GenUtil (fromGen)
import FlexTask.YesodConfig (Rendered, Widget)
import Horn (makeHornFormula, startAlgorithm)

import Global
import TaskSettings


data Label = Step Int | Output | Model | Satisfiable | Unsatisfiable

instance RenderMessage a Label where
    renderMessage _ ("de":_) (Step n)      = pack ("Schritt " ++ show n ++ ":")
    renderMessage _ ("de":_) Output        = "Ausgabe:"
    renderMessage _ ("de":_) Model         = "Modell:"
    renderMessage _ ("de":_) Satisfiable   = "erfüllbar"
    renderMessage _ ("de":_) Unsatisfiable = "unerfüllbar"
    renderMessage _ _        (Step n)      = pack ("Step " ++ show n ++ ":")
    renderMessage _ _        Output        = "Output:"
    renderMessage _ _        Model         = "Model:"
    renderMessage _ _        Satisfiable   = "satisfiable"
    renderMessage _ _        Unsatisfiable = "unsatisfiable"

getTask :: MonadRandom m => m (TaskData, String, Rendered Widget)
getTask = fromGen $ do
    formula <- makeHornFormula spirit extra
    let (steps, output, model) = startAlgorithm formula
        solution = Solution
            { correctSteps = steps
            , correctOutput = output
            , correctModel = model
            }
    pure (TaskData {formula, solution}, checkers, form)

form :: Rendered Widget
form = formify (Nothing :: Maybe ([Maybe String], SingleChoiceSelection, Maybe String))
    [ [ list Vertical (map (fieldSettingsLabel . Step) [1..stepFields])]
    , [ dropdown (fieldSettingsLabel Output)
        [ "---"
        , SomeMessage Satisfiable
        , SomeMessage Unsatisfiable
        ]
    , single (fieldSettingsLabel Model)
    ] ]

checkers :: String
checkers = [i|

{-\# OPTIONS_GHC -Wno-unused-record-wildcards \#-}
{-\# LANGUAGE ApplicativeDo \#-}
{-\# LANGUAGE RecordWildCards \#-}
{-\# LANGUAGE LambdaCase \#-}

module Check (checkSyntax, checkSemantics) where

import Data.Foldable (traverse_)
import Data.List (intersperse, intercalate, sort, (\\\\))
import Data.Map (Map)
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import Data.Tuple.Extra (second)

import Control.Monad (unless, when)
import Control.Monad.State (State)
import Control.OutputCapable.Blocks
import FlexTask.Generic.Form (getAnswer)
import Horn
import LogicTasks.Formula (TruthValue(..))
import Trees.Types (BinOp(..), SynTree(..))
import Trees.Print (simplestDisplay)

import qualified Data.List.Extra as List (replace)

import Global


checking :: OutputCapable m => Bool -> State (Map Language String) () -> LangM m
checking condition msg = when condition $ refuse $ indent $ translate msg

buildLatex :: OutputCapable m => SynTree BinOp Char -> [(Int, [Char])] -> LangM m
buildLatex formula steps = do
    paragraph $ indent $ latex $ foldr (\\(i,c) acc -> List.replace [c] ("\\\\underline{" ++ [c] ++ "^" ++ show i ++ "}") acc)
        (simplestDisplay formula) $ concatMap (\\(i,s) -> map (i,) s) steps
    paragraph $ traverse_ (\\(i,c) -> indent $ translate $ do
        german $  "Schritt " ++ show i ++ ": " ++ intersperse ',' c ++ "\\n"
        english $ "Step " ++ show i ++ ": " ++ intersperse ',' c ++ "\\n") steps
    pure ()

checkSyntax :: OutputCapable m => FilePath -> TaskData -> Submission -> LangM m
checkSyntax _ TaskData{..} Submission{..} = do
    checking (all isNothing steps && isNothing model) $ do
        german "Es dürfen nicht alle Texteingabefelder leer sein."
        english "Not all text input fields may be left blank."
    checking (any isJust (dropWhile isJust steps)) $ do
        german "Es dürfen keine Schritte übersprungen werden."
        english "Steps cannot be skipped."
    checking (getAnswer output == Just 1) $ do
        german "Es muss eine Ausgabe für den Algorithmus ausgewählt werden."
        english "An output option needs to be selected."
    checking (getAnswer output == Just 3 && isJust model) $ do
        german $ "Widerspruch gefunden: " ++
            "Die Formel sei unerfüllbar, dennoch wurde eine erfüllende Belegung (Modell) angegeben."
        english $ "Contradiction found: " ++
            "The formula is said to be unsatisfiable, yet a satisfying assignment (model) was provided."

    paragraph $ translate $ do
        german "Die Einsendung wird interpretiert als:"
        english "The submission is interpreted as:"
    buildLatex formula stepsSubmitted

    paragraph $ indent $ do
        translate $ do
          german $ germanOutput ++ germanWith
          english $ englishOutput ++ englishWith
        modelDisplay
        pure ()
    #{if printFeedbackImmediately then feedbackCode else ""}

    pure ()
  where
    stepsSubmitted = zip [1..] (map unCharAnswer (catMaybes steps))
    (germanOutput, englishOutput) = case getAnswer output of
      Just 3 -> ("\\"unerfüllbar\\"","\\"unsatisfiable\\"")
      Just 2 -> ("\\"erfüllbar\\"","\\"satisfiable\\"")
      _      -> ("","")
    (germanWith, englishWith, modelDisplay) = case model of
      Nothing -> ("","",pure ())
      Just m  -> ( ", mit"
                 , ", with"
                 , latex $ intercalate ",\\\\ " $
                     map (\\(c,w) -> "\\\\alpha(" ++ [c] ++  ")=" ++ show (fromEnum $ truth w)) m
                 )


checkSemantics :: OutputCapable m => FilePath -> TaskData -> Submission -> Rated m
checkSemantics _ TaskData{solution = Solution{..},..} Submission{..} = do
    let (altSteps,_,altModel) = newSolution formula stepsSubmitted
    let stepsCorrect = map (sort . snd) altSteps == map (sort . snd) stepsSubmitted
    yesNo stepsCorrect $ translate $ do
        german "Schritte richtig?"
        english "Steps correct?"
    let outputCorrect = Just (if correctOutput then 2 else 3) == getAnswer output
    yesNo outputCorrect $ translate $ do
        german "Ausgabe richtig?"
        english "Output correct?"
    let modelCorrect = altModel == sort (maybe [] (map (second truth)) model)
    yesNo modelCorrect $ translate $ do
        german "Modell richtig?"
        english "Model correct?"
    let points = (if stepsCorrect then 0.4 else 0.0) +
                 (if outputCorrect then 0.2 else 0.0) +
                 (if modelCorrect then 0.4 else 0.0)
    #{if showSolution then solutionCode else ""}
    #{if printFeedbackImmediately then "" else feedbackCode}

    unless (points >= #{minimumPoints}) $ refuse $ pure ()
    return points
  where
    stepsSubmitted = zip [1 :: Int ..] (map unCharAnswer (catMaybes steps))
|]
  where
    feedbackCode :: String
    feedbackCode = [i|
    let marked = map unCharAnswer $ catMaybes steps
        clauses = getClauses formula
        allAtomics = sort $ getAllAtomics clauses
    checking ((sort (concat marked) \\\\ allAtomics) /= []) $ do
        german $ "Hinweis: Markiert werden können nur jene atomare Aussagen, " ++
            "welche in der Formel vorkommen und nicht bereits markiert worden sind."
        english "Hint: Only those atomic propositions that occur in the formula and are not yet marked can be marked."
    checking (any ((> 1) . length) (drop 1 marked)) $ do
        german "Hinweis: Mit Ausnahme von Schritt 1, darf immer nur eine Aussage pro Schritt markiert werden."
        english "Hint: Apart from step 1, exactly one proposition may be marked in each step."
    checking ((\\case
        (Just step : _) -> sort (getFacts clauses) /= sort (unCharAnswer step)
        _               -> False) steps) $ do
            german "Hinweis: Im ersten Schritt müssen alle Fakten markiert werden."
            english "Hint: In the first step, all facts must be marked."
    checking (isJust model && sort (map fst $ fromJust model) /= allAtomics) $ do
        german "Hinweis: Im Modell muss jede atomare Teilformel genau einmal vorkommen."
        english "Hint: The model must contain each atomic subformula exactly once."
    |]

    solutionCode :: String
    solutionCode = [i|
    when (points /= 1) $ paragraph $ do
        translate $ do
            english "A correct solution is:"
            german "Eine korrekte Lösung ist:"
        buildLatex formula correctSteps
        paragraph $ do
            indent $ translate $ do
                german $ if correctOutput then "\\"erfüllbar\\", mit" else "\\"unerfüllbar\\""
                english $ if correctOutput then "\\"satisfiable\\", mit" else "\\"unsatisfiable\\""
            case correctModel of
                [] -> pure ()
                m  -> indent $ latex $ intercalate ",\\\\ " $
                    map (\\(c,w) -> "\\\\alpha(" ++ [c] ++  ")=" ++ show (fromEnum w)) (sort m)
            pure ()
        pure ()
|]


=============================================


{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Description (description) where

import Control.OutputCapable.Blocks
import Trees.Print (simplestDisplay)

import Global


description :: OutputCapable m => FilePath -> TaskData -> LangM m
description _ TaskData{..} = do
    paragraph $ translate $ do
        german "Gegeben ist eine Hornformel, welche sich bereits in Implikationsschreibweise befindet:"
        english "A Horn formula is given, which is already in implicational form."
    indent $ latex (simplestDisplay formula)
    paragraph $ translate $ do
        german "Wenden Sie den Markierungsalgorithmus aus der Vorlesung auf die Formel an."
        english "Apply the marking algorithm from the lecture to the formula."
    indent $ translate $ do
        german "Vervollständigen Sie das Protokoll."
        english "Complete the protocol."
    paragraph $ translate $ do
        english "Use the following notations:"
        german "Beachten Sie die folgenden Schreibweisen:"
    indent $ translate $ do
        english "Example of a listing of the propositions marked in a step:"
        german "Beispiel für die Angabe der in einem Schritt markierten Aussagen:"
    code "A, B"
    indent $ translate $ do
        english "Example of a Model:"
        german "Beispiel für die Angabe eines Modells:"
    code "A:1, B:0"
    paragraph $ translate $ do
        german "Eingabefelder, die für die Lösung nicht erforderlich sind, können freigelassen werden."
        english "You can leave input fields blank if they’re not needed for the solution."
    pure ()


=============================================


{-# LANGUAGE NamedFieldPuns #-}

module Parse (parseSubmission) where

import Text.Parsec

import Control.OutputCapable.Blocks
  ( LangM'
  , ReportT
  , OutputCapable
  )
import Control.OutputCapable.Blocks.Generic (($>>=))
import FlexTask.Generic.Parse
  ( Parse(..)
  , escaped
  , formParser
  , parseWithOrReport
  , reportWithFieldNumber
  )
import LogicTasks.Formula (TruthValue(..))

import qualified LogicTasks.Parsing as P

import Global


instance Parse CharAnswer where
  formParser = escaped $ do CharAnswer
     <$>    (do spaces
                c <- satisfy (`elem` ['A' .. 'Z'])
                spaces
                pure c) `sepBy` char ','

instance Parse [(Char,TruthValue)] where
  formParser = escaped $ do
    result <- tupleParser `sepBy` char ','
    pure result
    where
      tupleParser = do
        spaces
        c <- satisfy (`elem` ['A'..'Z'])
        spaces
        _ <- char ':'
        spaces
        t <- P.parser
        spaces
        pure (c,t)

parseSubmission ::
  (Monad m, OutputCapable (ReportT o m))
  => String
  -> LangM' (ReportT o m) Submission
parseSubmission input = parseWithOrReport formParser reportWithFieldNumber input $>>= \(steps, output, model) ->
  pure $ Submission {steps, output, model}
