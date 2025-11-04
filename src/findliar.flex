
taskName: FindLiar

=============================================

{-# LANGUAGE DeriveDataTypeable#-}

module Global where

import Data.Data (Data)
import Data.Text (Text)

import LogicTasks.Formula (TruthValue)
import Trees.Types (SynTree, BinOp, PropFormula)


newtype Table = Table [(Maybe (PropFormula Char), [Maybe TruthValue])]
    deriving (Eq,Show)

data Namen = A | B | C
    deriving (Eq,Enum,Bounded,Show,Ord,Data)

allNamen :: [Namen]
allNamen = [minBound .. maxBound]

data Submission = Submission
  { submittedParts   :: [PropFormula Char]
  , submittedFormula :: PropFormula Char
  , submittedTable   :: Table
  , identifiedLiars  :: [Namen]
  }

data TaskData = TaskData
  { hints          :: [Text]
  , solutionValues :: [Bool]
  , formulaParts   :: [SynTree BinOp Char]
  } deriving (Data)


=============================================


module TaskSettings where

import Control.OutputCapable.Blocks
  ( LangM
  , OutputCapable
  , indent
  , refuse
  , text
  )


emptyColumns, staticColumns, staticColsEnd, totalColumns, rows :: Int
emptyColumns  = 4
staticColumns = 3
staticColsEnd = 1
totalColumns  = staticColumns + emptyColumns + staticColsEnd
rows          = 2 ^ staticColumns

showSolution, printFeedbackImmediately :: Bool
showSolution             = True
printFeedbackImmediately = True

validateSettings :: OutputCapable m => LangM m
validateSettings
  | emptyColumns < 3 = refuse $ indent $ text
      "Die Anzahl der leeren Spalten ist kleiner als die Anzahl der Hinweise."
  | totalColumns > 18 = refuse $ indent $ text $
      "Die Tabelle enthält zu viele Spalten. " ++
      "Das Eingabeformular kann bei über 18 Spalten nicht mehr korrekt angezeigt werden."
  | otherwise = pure ()


=============================================


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module TaskData (getTask) where

import Control.Monad.Random (MonadRandom)
import Data.Char (digitToInt)
import Data.List (sortOn, transpose)
import Data.String (fromString)
import Data.String.Interpolate (i)
import Numeric (showBin)
import Test.QuickCheck.Gen
import Yesod

import FlexTask.FormUtil
  (($$>), addCss, addCssClass)
import FlexTask.Generic.Form
  ( Alignment(..)
  , Formify(..)
  , formify
  , formifyInstanceMultiChoice
  , list
  , single
  , buttonsEnum
  )
import FlexTask.GenUtil (fromGen)
import FlexTask.YesodConfig (Rendered, Widget)
import LogicTasks.Formula (TruthValue(..))
import LogicTasks.Forms (tableForm)
import FindLiarTask

import Global
import TaskSettings



getTask :: MonadRandom m => m (TaskData, String, Rendered Widget)
getTask = fromGen $ do
    d@(x, y, z, _) <- taskData
    let (formulas, unsortedHints) = makeHintsAndFormula d
        dataSortedOnName = sortOn fst [x, y, z]
        (formulaParts, hints) = unzip $ sortOn snd $ zip formulas unsortedHints
        solutionValues = map snd dataSortedOnName
    pure ( TaskData {hints, solutionValues, formulaParts}
         , checkers
         , form
         )
  where
    form = addCss formulaCss $
      formify (Nothing :: Maybe ([String], String))
        [ [list Vertical [ "1) ", "2) ", "3) "]]
        , [single $ addCssClass "formula-input" "Gesamtformel F="]
        ] $$>
      tableForm emptyColumns rows ["A","B","C"] ["F"] $$>
      formify (Nothing :: Maybe [Namen])
        [[buttonsEnum Vertical "Wer lügt?" (fromString . show @Namen)]]

    formulaCss = [cassius|
      .formula-input
        width: 42%
      th, td
        border: 1px solid black
        border-collapse: collapse
        text-align: center
      table tr th:nth-child(-n+3)
        width: 20px
      table tr td:nth-child(-n+3)
        height: 25px
      table tr th:nth-child(n+4)
        width: 125px
      table tr th:nth-child(n+8)
        width: 25px
      table tr th:last-child
        padding: 10px 0px
      |]

instance Formify [Namen] where
  formifyImplementation = formifyInstanceMultiChoice

checkers :: String
checkers = [i|
{-\# OPTIONS_GHC -Wno-unused-record-wildcards \#-}
{-\# LANGUAGE ApplicativeDo \#-}
{-\# LANGUAGE OverloadedStrings \#-}
{-\# LANGUAGE RecordWildCards \#-}

module Check (checkSemantics, checkSyntax) where

import Control.Monad (when)
import Data.Containers.ListUtils (nubOrd)
import Data.List (intercalate, transpose)
import Data.Maybe (fromJust)
import Data.Ratio ((%))
import Data.Text (Text, unpack)

import Control.OutputCapable.Blocks
import LogicTasks.Formula
  ( Formula(..)
  , TruthValue(..)
  , isSemanticEqual
  )
import Trees.Formula ()
import Trees.Print (simplestDisplay)
import Trees.Types
  ( SynTree(..)
  , BinOp(..)
  , toSynTree
  )

import Global


toAllocationInverted :: [Namen] -> [(Char,Bool)]
toAllocationInverted liars =
  map (\\x -> (nameMatch x,  x `notElem` liars)) allNamen
  where
    nameMatch A = 'A'
    nameMatch B = 'B'
    nameMatch C = 'C'

printAllocation :: [(Char,Bool)] -> String
printAllocation =
  intercalate ", " . map (\\(a,b) ->
    [a] ++ "=" ++ show (if b then (1 :: Int) else (0 :: Int)))

feedbackCompareHints :: OutputCapable m => Text -> [Namen] -> LangM m
feedbackCompareHints unmatchedHint identifiedLiars = do
  indent $ text $
    "Widerspruch gefunden: Die ausgewählten Lügner sind: " ++
    show identifiedLiars ++ "." ++
    " Das passt jedoch nicht zu dem Hinweis: " ++
    unpack unmatchedHint

feedbackCompareChosenLiars :: OutputCapable m => [(Char, Bool)] -> SynTree BinOp Char -> LangM m
feedbackCompareChosenLiars allocationFromLiars wrongLiar = do
  indent $ text $
    "Widerspruch gefunden: Die zu der getroffenen Lügner-Auswahl passende Belegung ist: " ++
    printAllocation allocationFromLiars ++ "." ++
    " Jedoch wertet die angegebene Teilformel: " ++
    simplestDisplay wrongLiar ++
    " unter dieser Belegung zu 0 (falsch) aus."

checkSyntax :: OutputCapable m => FilePath -> TaskData -> Submission -> LangM m
checkSyntax _ TaskData{..} Submission{..} = do
  when (nubOrd atomicRows /= atomicRows) $
    refuse $ indent $ text $
      "Mindestens eine Belegung kommt in der Wahrheitstafel mehrfach vor. " ++
      "Es müssen alle Belegungen genau einmal vorkommen."
  let theTable = #{startingTable} --ignore-length
  when (atomicCols == map reverse theTable) $
    refuse $ indent $ text $
      "Die Spalten der atomaren Formeln sind invertiert. " ++
      "Bitte legen Sie die Tafel so an wie in der Vorlesung vorgegeben."
  assertion (atomicCols == theTable) $ text
    "Spalten der atomaren Formeln ergeben eine korrekte (aufsteigend geordnete) Wahrheitstafel?"
  #{if printFeedbackImmediately then findContradictions "map toSynTree submittedParts" else ""}
  pure ()
  where
    Table xs   = submittedTable
    columns    = map snd xs
    atomicCols = take #{staticColumns} columns
    atomicRows = filter (notElem Nothing) $ transpose atomicCols

truthValuesToBinaryToIndex :: [Bool] -> Int
truthValuesToBinaryToIndex =
  foldl (\\acc value -> acc * 2 + if value then 1 else 0) 0

generateTruthTable :: [Bool] -> [[Maybe TruthValue]]
generateTruthTable solutionValues =
  let solIndex = truthValuesToBinaryToIndex solutionValues
  in [[ if i == solIndex
          then Just (TruthValue True)
          else Just (TruthValue False)
      | i <- [0..7] ]]

checkSemantics :: OutputCapable m => FilePath -> TaskData -> Submission -> Rated m
checkSemantics _ TaskData{..} Submission{..} = do
  let correctParts = zipWith isSemanticEqual formulaParts submittedTrees
  yesNo (and correctParts) $ text
    "Aussagen sind korrekt übersetzt?"
  let correctFormula = isSemanticEqual (toSynTree submittedFormula) solutionFormula ||
                       isSemanticEqual (toSynTree submittedFormula) (foldr1 (Binary And) submittedTrees)
  yesNo correctFormula $ text
    "Gesamtformel ist korrekt?"
  #{if not printFeedbackImmediately then findContradictions "submittedTrees" else ""}
  let correctLast = drop #{staticColumns+emptyColumns} columns == generateTruthTable solutionValues
  yesNo correctLast $ text
    "Spalte F der Wahrheitstafel ist korrekt?"
  let correctNames = identifiedLiars == listOfLiars
  yesNo correctNames $ text
    "Auswahl der Lügner ist korrekt?"
  let points = (if and correctParts then 0.4 else 0.0) +
               (if correctFormula   then 0.2 else 0.0) +
               (if correctLast      then 0.2 else 0.0) +
               (if correctNames     then 0.2 else 0.0)
  res <- printSolutionAndAssertMinimum (MinimumThreshold (1 % 4)) IndefiniteArticle maybeAnswer points
  pure res
  where
    Table xs        = submittedTable
    submittedTrees  = map toSynTree submittedParts
    solutionFormula = foldr1 (Binary And) formulaParts
    listOfLiars     = map fst $ filter (not . snd) (zip allNamen solutionValues)
    columns         = map snd xs
    maybeAnswer     = #{if showSolution then "Just (" ++ solutionCode ++ ")" else "Nothing"}
|]
  where
    solutionCode = [i|"Formel: " ++ simplestDisplay solutionFormula ++ "\\nKorrekte Einträge in Wahrheitstafel.\\nLügner: " ++ show listOfLiars|]

    findContradictions :: String -> String
    findContradictions submittedTrees = [i|
  let allocationFromLiars = toAllocationInverted identifiedLiars
      evaluated           = not . fromJust . evaluate allocationFromLiars
      wrongLiars          = [formula | formula <- #{submittedTrees}, evaluated formula]
  case wrongLiars of
    liar:_ -> refuse $ feedbackCompareChosenLiars allocationFromLiars liar
    [] -> pure ()
  let unmatchedHints = [hint | (formula, hint) <- zip formulaParts hints, evaluated formula]
  case unmatchedHints of
    hint:_ -> refuse $ feedbackCompareHints hint identifiedLiars
    [] -> pure ()
    |]

startingTable :: [[Maybe TruthValue]]
startingTable = map (Just . TruthValue . toEnum . digitToInt) <$>
  transpose (pad . (`showBin` "") <$> [0 .. rows - 1])
  where pad s = replicate (staticColumns - length s) '0' ++ s


=============================================


{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Description (description) where

import Data.Text (unpack)

import Control.OutputCapable.Blocks
import LogicTasks.Keys (keyHeading, basicOpKey, arrowsKey)

import Global


description :: OutputCapable m => FilePath -> TaskData -> LangM m
description _ TaskData{..} = do
  paragraph $ text
    ("Ein Logiker irrt auf dem Campus herum, um seinen Hörsaal zu finden. " ++
      "Er fragt drei Studierende (mit den ausgefallenen Namen A, B, C), wo er den Raum LB 107 finden kann. " ++
      "Diese Studierenden sagen (jeweils) immer die Wahrheit oder lügen immer. " ++
      "Um die Zuverlässigkeit der Antworten zu überprüfen, fragt er die drei Studierenden, ob die anderen lügen, " ++
      "und erhält folgende Antworten:")
  enumerateM (text . (++ ")") . show) (zip [1 ::Int ..] (map (text . unpack) hints))
  paragraph $ text
    ("Übersetzen Sie die Hinweise und die Kombination dieser jeweils in eine aussagenlogische Formel. " ++
      "Geben Sie die Formeln in die entsprechend benannten Textfelder ein.")
  paragraph $ text
    ("Wer lügt? Leiten Sie Ihr Ergebnis mittels Wahrheitstafel her. " ++
      "Füllen Sie dabei alle benannten Spalten vollständig aus. " ++
      "Sie können Hilfsspalten verwenden. Kreuzen Sie anschließend die entsprechenden Namen an.")
  paragraph $ text
    ("Verwenden Sie dabei die atomaren Formeln A, B, C mit der Interpretation, " ++
    "dass eine Zuordnung von 'wahr' dafür steht, dass die entsprechende Person die Wahrheit sagt.")
  keyHeading
  basicOpKey True
  arrowsKey
  pure ()


=============================================


{-# LANGUAGE NamedFieldPuns #-}

module Parse (parseSubmission) where

import Data.List.Extra (chunksOf, transpose)

import Control.OutputCapable.Blocks
import Control.OutputCapable.Blocks.Generic (($>>=))
import Formula.Parsing.Delayed
  ( complainAboutMissingParenthesesIfNotFailingOn )
import FlexTask.Generic.Parse
  ( Parse(..)
  , displayInputAnd
  , escaped
  , parseInstanceMultiChoice
  , parseWithFallback
  , parseWithOrReport
  , reportWithFieldNumber
  )
import LogicTasks.Formula (TruthValue)
import ParsingHelpers (fully)
import Trees.Parsing ()
import Trees.Types (PropFormula(..))

import qualified Formula.Parsing as FP

import Global
import TaskSettings


instance Parse TruthValue where
  formParser = escaped FP.parser

instance Parse [Namen] where
  formParser = parseInstanceMultiChoice

makeTable :: [Maybe (PropFormula Char)] -> [Maybe TruthValue] -> Table
makeTable headers values = Table $ zip allHeaders formattedTruthValues
  where
    allHeaders = map (Just . Atomic) "ABC" ++ headers ++ [Just $ Atomic 'F']
    formattedTruthValues = transpose $ chunksOf totalColumns values

parseSubmission :: (Monad m, OutputCapable (ReportT o m)) => String -> LangM' (ReportT o m) Submission
parseSubmission input =
  parseWithOrReport formParser reportWithFieldNumber input $>>= \(fs, f, headers, columns, identifiedLiars) ->
    traverse parseIt fs $>>= \submittedParts ->
      parseIt f $>>= \submittedFormula ->
        traverse (traverse parseIt) headers $>>= \parsedHeaders ->
          pure $ Submission {
            submittedParts,
            submittedFormula,
            submittedTable = makeTable parsedHeaders columns,
            identifiedLiars
            }
  where
    parseIt = parseWithFallback
      (fully FP.parser)
      (displayInputAnd complainAboutMissingParenthesesIfNotFailingOn)
      (fully FP.formulaSymbolParser)
