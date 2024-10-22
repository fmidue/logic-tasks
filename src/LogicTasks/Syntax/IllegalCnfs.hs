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
import Data.Map as Map (Map,fromAscList)
import LogicTasks.Helpers
import Tasks.LegalNormalForm.Config(LegalNormalFormConfig(..), LegalNormalFormInst(..), checkLegalNormalFormConfig)




descriptionTemplate :: OutputCapable m => Map Language String -> LegalNormalFormInst -> LangM m
descriptionTemplate what LegalNormalFormInst{..} = do
    instruct $ do
      english "Consider the following propositional logic formulas:"
      german "Betrachten Sie die folgenden aussagenlogischen Formeln:"

    focus $ unlines $ indexed formulaStrings

    instruct $ do
      english $ "Which of these formulas are given in " ++ localise English what ++ "?"
      german $ "Welche dieser Formeln sind in " ++ localise German what ++ " angegeben?"

    instruct $ do
      english $  "Enter a list containing the indices of the formulas in " ++ localise English what ++ " to submit your answer."
      german $ "Geben Sie eine Liste der Indizes aller in " ++ localise German what ++ " vorliegender Formeln als Ihre Lösung an."

    example "[2,3]" $ do
      english $ "For example, if only choices 2 and 3 are given in " ++ localise English what ++ ", then the solution is:"
      german $ "Liegen beispielsweise nur Auswahlmöglichkeiten 2 und 3 in " ++ localise German what ++ " vor, dann ist diese Lösung korrekt:"

    extra addText

    pure ()

description :: OutputCapable m => LegalNormalFormInst -> LangM m
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
partialGrade LegalNormalFormInst{..} = multipleChoiceSyntax False [1..length formulaStrings]


completeGrade :: OutputCapable m => LegalNormalFormInst -> [Int] -> Rated m
completeGrade LegalNormalFormInst{..} = multipleChoice DefiniteArticle what solutionDisplay (Map.fromAscList solution)
  where
    what = translations $ do
      german "Indizes"
      english "indices"
    solutionDisplay | showSolution = Just $ show [ i | (i, True) <- solution ]
                    | otherwise = Nothing
    solution = map (\i -> (i, i `notElem` serialsOfWrong)) [1..length formulaStrings]
