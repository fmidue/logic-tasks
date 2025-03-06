{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module LogicTasks.Syntax.IllegalCnfs where


import Control.OutputCapable.Blocks (
  LangM,
  OutputCapable,
  english,
  german,
  Rated,
  multipleChoice,
  ArticleToUse (DefiniteArticle),
  translations,
  multipleChoiceSyntax,
  Language (..),
  localise,
  reRefuse,
  GenericOutputCapable (indent, code),
  )
import Control.Monad (when)
import Data.Map as Map (Map,fromAscList)
import LogicTasks.Helpers
import Tasks.LegalNormalForm.Config(
  LegalNormalFormConfig(..),
  LegalNormalFormInst(..),
  checkLegalNormalFormConfig,
  treeIsErroneous,
  TreeInfo (..),
  ErrorReason (..),
  )
import Data.Tuple.Extra (thd3)
import Control.Applicative (Alternative)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isJust)
import Data.List.Extra (nubSort)




descriptionTemplate :: OutputCapable m => Map Language String -> Bool -> LegalNormalFormInst -> LangM m
descriptionTemplate what inputHelp LegalNormalFormInst{..} = do
    instruct $ do
      english "Consider the following propositional logic formulas:"
      german "Betrachten Sie die folgenden aussagenlogischen Formeln:"

    focus $ unlines $ indexed $ map thd3 formulaInfos

    instruct $ do
      english $ "Which of these formulas are given in " ++ localise English what ++ "?"
      german $ "Welche dieser Formeln liegen in " ++ localise German what ++ " vor?"

    when inputHelp $ do
      instruct $ do
        english $  "Enter a list containing the indices of the formulas given in " ++ localise English what ++ " to submit your answer."
        german $ "Geben Sie eine Liste der Indizes aller in " ++ localise German what ++ " vorliegender Formeln als Ihre Lösung an."

      example "[2,3]" $ do
        english $ "For example, if only choices 2 and 3 are given in " ++ localise English what ++ ", then the correct solution is:"
        german $ "Liegen beispielsweise nur Auswahlmöglichkeiten 2 und 3 in " ++ localise German what ++ " vor, dann ist diese Lösung korrekt:"
      pure ()

    extra addText

    pure ()

description :: OutputCapable m => Bool -> LegalNormalFormInst -> LangM m
description = descriptionTemplate $ translations $ do
  german "konjunktiver Normalform (KNF)"
  english "conjunctive normal form (cnf)"


verifyInst :: OutputCapable m => LegalNormalFormInst -> LangM m
verifyInst _ = pure ()



verifyConfig :: OutputCapable m => LegalNormalFormConfig -> LangM m
verifyConfig = checkLegalNormalFormConfig



start :: [Int]
start = []



partialGrade :: OutputCapable m => LegalNormalFormInst -> [Int] -> LangM m
partialGrade LegalNormalFormInst{..} = multipleChoiceSyntax False [1..length formulaInfos]


completeGrade :: (OutputCapable m, Alternative m, Monad m) => LegalNormalFormInst -> [Int] -> Rated m
completeGrade LegalNormalFormInst{..} sol = reRefuse
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

      instruct $ case info of
        Correct -> do
          german "liegt in korrekter Form vor."
          english "is given in correct shape."
        CorrectSingleClause -> do
          german "liegt in korrekter Form vor. "
          german "Es handelt sich hierbei um eine einzige Klausel."
          english "is given in correct shape. "
          english "The formula consists of one clause."
        CorrectAtomicClauses -> do
          german "liegt in korrekter Form vor. "
          german "Die Klauseln verfügen hier nur über ein einzelnes Literal."
          english "is given in correct shape. "
          english "The clauses consist of a single literal."
        Erroneous err -> do
          german "liegt nicht in korrekter Form vor. "
          english "is not given in correct shape. "
          case err of
            OnClauseLevel IllegalNegation -> do
              german "Auf Klausel-Ebene findet eine illegale Negation statt."
              english "There exists an illegal negation on clause level."
            OnClauseLevel IllegalOperator -> do
              german "Auf Klausel-Ebene existiert ein illegaler Operator."
              english "There exists an illegal operator on clause level."
            IllegalNegation -> do
              german "Auf äußerster Ebene findet eine illegale Negation statt."
              english "There exists an illegal negation on the outermost level."
            IllegalOperator -> do
              german "Auf äußerster Ebene existiert ein illegaler Operator."
              english "There exists an illegal operator on the outermost level."
            _ -> pure ()

      pure ()

    pure ()
  where
    detailedSolution = fromMaybe False showSolution
    what = translations $ do
      german "Indizes"
      english "indices"
    solution = map (\(i,info,_) -> (i, not (treeIsErroneous info))) formulaInfos
    hasWrongSolution = filter snd solution /= nubSort (map (,True) sol)
    simpleSolutionDisplay
      | isJust showSolution && not detailedSolution = Just $ show [ i | (i,True) <- solution]
      | otherwise = Nothing
