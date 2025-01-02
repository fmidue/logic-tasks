{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
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
  )
import Control.Monad (when)
import Data.Map as Map (Map,fromAscList)
import LogicTasks.Helpers
import Tasks.LegalNormalForm.Config(LegalNormalFormConfig(..), LegalNormalFormInst(..), checkLegalNormalFormConfig, treeIsErroneous)
import Data.Tuple.Extra (thd3)




descriptionTemplate :: OutputCapable m => Map Language String -> Bool -> LegalNormalFormInst -> LangM m
descriptionTemplate what inputHelp LegalNormalFormInst{..} = do
    instruct $ do
      english "Consider the following propositional logic formulas:"
      german "Betrachten Sie die folgenden aussagenlogischen Formeln:"

    focus $ unlines $ indexed $ map thd3 formulas

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
partialGrade LegalNormalFormInst{..} = multipleChoiceSyntax False [1..length formulas]


completeGrade :: OutputCapable m => LegalNormalFormInst -> [Int] -> Rated m
completeGrade LegalNormalFormInst{..} = multipleChoice DefiniteArticle what solutionDisplay (Map.fromAscList solution)
  where
    what = translations $ do
      german "Indizes"
      english "indices"
    solutionDisplay | showSolution = Just $ show [ i | (i, True) <- solution ]
                    | otherwise = Nothing
    solution = map (\(i,info,_) -> (i, not (treeIsErroneous info))) formulas
