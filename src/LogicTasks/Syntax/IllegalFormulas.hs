{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module LogicTasks.Syntax.IllegalFormulas where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (code, image, indent),
  LangM,
  OutputCapable,
  ($=<<),
  english,
  german,
  Rated,
  multipleChoice,
  ArticleToUse (DefiniteArticle),
  translations,
  reRefuse,
  multipleChoiceSyntax,
  )
import Data.List.Extra (nubSort)
import LogicTasks.Helpers (example, extra, focus, indexed, instruct)
import Tasks.LegalProposition.Config (
  LegalPropositionInst(..),
  LegalPropositionConfig(..),
  checkLegalPropositionConfig,
  propFormulaIsErroneous,
  PropFormulaInfo (..),
  PropErrorReason (..),
  )
import Control.Monad (when)
import Trees.Print (transferToPicture)
import Control.Monad.IO.Class (MonadIO (liftIO))
import LogicTasks.Syntax.TreeToFormula (cacheTree)
import Data.Foldable (for_)
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Map as Map (fromAscList)
import Control.Applicative (Alternative)
import Data.Tuple.Extra (thd3)




description :: OutputCapable m => Bool -> LegalPropositionInst -> LangM m
description inputHelp LegalPropositionInst{..} = do
    instruct $ do
      english "Consider the following propositional (pseudo) formulas:"
      german "Betrachten Sie die folgenden aussagenlogischen (Pseudo-)Formeln:"

    focus $ unlines $ indexed $ map thd3 formulaInfos

    instruct $ do
      english "Some of these are syntactically wrong. Which of these formulas are correctly formed?"
      german "Einige davon enthalten syntaktische Fehler. Geben Sie an, welche Formeln korrekt geformt sind."

    when inputHelp $ do
      instruct $ do
        english "Enter a list containing the indices of the syntactically correct formulas to submit your answer."
        german "Geben Sie eine Liste der Indizes aller syntaktisch korrekten Formeln als Ihre Lösung an."

      example "[2,3]" $ do
        english "For example, if only choices 2 and 3 are correctly formed, then the solution is:"
        german "Sind beispielsweise nur Auswahlmöglichkeiten 2 und 3 richtig geformt, dann ist diese Lösung korrekt:"
      pure()

    extra addText
    pure ()


verifyInst :: OutputCapable m => LegalPropositionInst -> LangM m
verifyInst _ = pure ()



verifyConfig :: OutputCapable m => LegalPropositionConfig -> LangM m
verifyConfig = checkLegalPropositionConfig



start :: [Int]
start = []



partialGrade :: OutputCapable m => LegalPropositionInst -> [Int] -> LangM m
partialGrade LegalPropositionInst{..} = multipleChoiceSyntax False [1..length formulaInfos]


-- jscpd:ignore-start
completeGrade
  :: (OutputCapable m, MonadIO m, Alternative m)
  => FilePath
  -> LegalPropositionInst
  -> [Int]
  -> Rated m
completeGrade path LegalPropositionInst{..} sol = reRefuse
  (multipleChoice
    DefiniteArticle
    what
    simpleSolutionDisplay
    (Map.fromAscList solution)
    sol)
  $ when (hasWrongSolution && detailedSolution) $ indent $ do

    instruct $ do
      german "Die Lösung dieser Aufgabe sieht wie folgt aus:"
      english "The solution for this task looks like this:"

    for_ formulaInfos $ \(i,info, formula) -> do

      code (show i ++ ". " ++ formula)

      case info of
        Correct tree -> do
          instruct $ do
            german "ist korrekt geformt. "
            german "Der zugehörige Syntaxbaum sieht so aus:"
            english "is correctly formed. "
            english "The associated syntax tree looks like this:"

          image $=<< liftIO $ cacheTree (transferToPicture tree) path

          pure ()
        Erroneous err -> do
          instruct $ do
            german "ist nicht korrekt geformt. "
            english "is not correctly formed. "
            case err of
              IllegalParentheses -> do
                german "Die Anzahl an öffnenden und schließenden Klammern stimmt nicht überein."
                english "The amount of opening and closing parentheses does not match."
              IllegalOperator -> do
                german "Es werden zwei Teilformen falsch miteinander verknüpft."
                english "Two subformulas are combined incorrectly."
              IllegalOperand -> do
                german "Nicht alle Operatoren verfügen über gültige Operanden."
                english "Not all operators have valid operands."
              MissingOperator -> do
                german "Nicht alle Teilformen werden verknüpft."
                english "There are uncombined subformulas."
              MissingOperand -> do
                german "Nicht alle Operatoren verfügen über die korrekte Anzahl an Operanden."
                english "Not all operators have the correct number of operands."

      pure ()

    instruct $ do
      german "Hinweis: Für manche Fehler gibt es auch andere Interpretationen."
      english "Note: There are also other interpretations for some errors."

    pure ()

    where
      detailedSolution = fromMaybe False showSolution
      what = translations $ do
        german "Indizes"
        english "indices"
      solution = map (\(i,info,_) -> (i, not (propFormulaIsErroneous info))) formulaInfos
      hasWrongSolution = filter snd solution /= nubSort (map (,True) sol)
      simpleSolutionDisplay
        | isJust showSolution && not detailedSolution = Just $ show [ i | (i,True) <- solution]
        | otherwise = Nothing
-- jscpd:ignore-end
