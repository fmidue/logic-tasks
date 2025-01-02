{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.SubTreeSet where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  ($=<<),
  english,
  german,
  translate,
  localise,
  translations,
  Rated,
  extendedMultipleChoice,
  MinimumThreshold (MinimumThreshold),
  Punishment (Punishment),
  TargetedCorrect (TargetedCorrect),
  ArticleToUse (IndefiniteArticle),
  reRefuse,
  )
import Data.List (intercalate, nub, sort)
import qualified Data.Set (map)
import qualified Data.Map as Map (fromSet, insert, filter)
import Data.Maybe (isNothing, fromJust)
import LogicTasks.Helpers (extra, focus, instruct, keyHeading, reject, basicOpKey, arrowsKey)
import Tasks.SubTree.Config (checkSubTreeConfig, SubTreeInst(..), SubTreeConfig(..))
import Trees.Types (FormulaAnswer(..))
import Trees.Print (display, transferToPicture)
import Trees.Helpers
import Control.Monad (when)
import LogicTasks.Syntax.TreeToFormula (cacheTree)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Foldable (for_)
import Formula.Parsing.Delayed (Delayed, parseDelayedWithAndThen, complainAboutMissingParenthesesIfNotFailingOn, withDelayedSucceeding)
import Formula.Parsing (Parse(..), formulaListSymbolParser)
import Control.Applicative (Alternative)
import GHC.Real ((%))


description :: OutputCapable m => Bool -> SubTreeInst -> LangM m
description withListInput SubTreeInst{..} = do
    instruct $ do
      english "Consider the following propositional logic formula:"
      german "Betrachten Sie die folgende aussagenlogische Formel:"

    focus (display tree)

    instruct $ do
      english $ "Give " ++ show inputTreeAmount ++ " non-atomic subformulas that are contained in this formula."
      german $ "Geben Sie " ++ show inputTreeAmount ++ " nicht-atomare Teilformeln an, die in dieser Formel enthalten sind."

    instruct $ do
      english "Submit your solution as a list of subformulas."
      german "Reichen Sie Ihre Lösung als eine Liste von Teilformeln ein."

    instruct $ do
      english "Remove bracket pairs which only serve to enclose an entire subformula you provide, and do not add any additional brackets."
      german "Entfernen Sie dabei Klammerpaare, die eine angegebene Teilformel komplett umschließen, und fügen Sie keine zusätzlichen Klammern hinzu."

    paragraph $ indent $ do
      translate $ do
        english "For example, if ¬(A ∨ (B ∧ C)) is the given formula and two subformulas are required, then a correct solution is:"
        german "Ist z.B. ¬(A ∨ (B ∧ C)) die gegebene Formel und es werden zwei Teilformeln gesucht, dann ist die folgende Lösung korrekt:"
      translatedCode $ flip localise $ translations exampleCode
      pure ()

    paragraph $ translate $ do
      german "Sie können dafür die ürsprüngliche Formel mehrfach in die Abgabe kopieren und Teile entfernen, oder leer startend die folgenden Schreibweisen nutzen:"
      english "You can copy the original formula into the submission several times and remove parts, or start from scratch and use the following syntax:"

    keyHeading
    basicOpKey unicodeAllowed
    when showArrowOperators arrowsKey

    extra addText
    pure ()
      where
        exampleCode = do
          german $ exampleForm ger
          english $ exampleForm eng

        (ger,eng)
          | unicodeAllowed = (["A ∨ (B ∧ C)", "B und C"] ,["A ∨ (B ∧ C)", "B and C"]) -- no-spell-check
          | otherwise = (["A oder (B und C)", "B und C"],["A or (B and C)", "B and C"]) -- no-spell-check

        exampleForm s
          | withListInput = "[ " ++ intercalate ", " s ++ " ]"
          | otherwise = intercalate "\n" s


verifyInst :: OutputCapable m => SubTreeInst -> LangM m
verifyInst _ = pure ()



verifyConfig :: OutputCapable m => SubTreeConfig -> LangM m
verifyConfig = checkSubTreeConfig



start :: [FormulaAnswer]
start = [FormulaAnswer Nothing]


partialGrade :: OutputCapable m => SubTreeInst -> Delayed [FormulaAnswer] -> LangM m
partialGrade = parseDelayedWithAndThen parser complainAboutMissingParenthesesIfNotFailingOn formulaListSymbolParser . partialGrade'

partialGrade' :: OutputCapable m => SubTreeInst -> [FormulaAnswer] -> LangM m
partialGrade' SubTreeInst{..} fs
    | any (isNothing . maybeForm) fs =
      reject $ do
        english "At least one of your answers is not a well-formed formula."
        german "Mindestens eine Ihrer Antworten ist keine wohlaufgebaute Formel."

    | any (`notElem` origLits) literals =
      reject $ do
        english "At least one formula in your submission contains unknown atomic formulas."
        german "Ihre Abgabe beinhaltet mindestens eine Formel mit unbekannten atomaren Formeln."

    | any (> origOpsNum) opsNum =
      reject $ do
        english "Your submission contains at least one formula with more logical operators than the original formula."
        german "Ihre Abgabe beinhaltet mindestens eine Formel mit mehr logische Operatoren als die ursprüngliche Formel."

    | amount < inputTreeAmount =
      reject $ do
        english "Your submission does not contain enough different subformulas. "
        english $ "Add " ++ show (inputTreeAmount - amount) ++ "."
        german "Ihre Abgabe beinhaltet nicht genügend verschiedene Teilformeln. "
        german $ "Fügen Sie " ++ show (inputTreeAmount - amount) ++ " hinzu."

    | amount > inputTreeAmount =
      reject $ do
        english "Your submission contains too many formulas."
        german "Ihre Abgabe enthält zu viele Formeln."

    | otherwise = pure ()
  where
    amount = fromIntegral $ length $ nub fs
    literals = sort $ nub $ concatMap (collectLeaves . fromJust . maybeForm) fs
    opsNum = map (numOfOpsInFormula . fromJust . maybeForm) fs
    origLits = sort $ nub $ collectLeaves tree
    origOpsNum = numOfOps tree


completeGrade
  :: (OutputCapable m, MonadIO m, Alternative m)
  => FilePath
  -> SubTreeInst
  -> Delayed [FormulaAnswer]
  -> Rated m
completeGrade path inst = completeGrade' path inst `withDelayedSucceeding` parser

completeGrade'
  :: (OutputCapable m, MonadIO m, Alternative m)
  => FilePath
  -> SubTreeInst
  -> [FormulaAnswer]
  -> Rated m
completeGrade' path SubTreeInst{..} sol = reRefuse
  (extendedMultipleChoice
    (MinimumThreshold (1 % inputTreeAmount))
    (Punishment 0)
    (TargetedCorrect (fromIntegral inputTreeAmount))
    IndefiniteArticle
    what
    Nothing
    solution
    submission)
  $ when showSolution $ indent $ do
    instruct $ do
      english ("A possible solution for this task contains " ++ show inputTreeAmount ++ " of the following subformulas:")
      german ("Eine mögliche Lösung für diese Aufgabe beinhaltet " ++ show inputTreeAmount ++ " der folgenden Teilformeln:")

    for_ correctTrees $ \x -> do
      code (display x)

      instruct $ do
        german "mit zugehörigem Teil-Syntaxbaum:"
        english "with associated partial syntax tree:"

      image $=<< liftIO $ cacheTree (transferToPicture x) path
      pure ()

    pure ()
    where
      what = translations $ do
        german "Teilformeln"
        english "subformulas"
      solution = Map.fromSet (const True) $ Data.Set.map display correctTrees
      submission = foldr ((`Map.insert` True) . show) (Map.filter not solution) sol
