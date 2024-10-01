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
  )
import Data.Map as Map (fromAscList)
import LogicTasks.Helpers
import Tasks.LegalCNF.Config(LegalCNFConfig(..), LegalCNFInst(..), checkLegalCNFConfig)




description :: OutputCapable m => LegalCNFInst -> LangM m
description LegalCNFInst{..} = do
    instruct $ do
      english "Consider the following propositional logic formulas:"
      german "Betrachten Sie die folgenden aussagenlogischen Formeln:"

    focus $ unlines $ indexed formulaStrings

    instruct $ do
      english "Which of these formulas are given in conjunctive normal form (cnf)?"
      german "Welche dieser Formeln sind in konjunktiver Normalform (KNF) angegeben?"

    instruct $ do
      english "Enter a list containing the indices of the cnf formulas to submit your answer."
      german "Geben Sie eine Liste der Indizes aller in KNF vorliegenden Formeln als Ihre Lösung an."

    example "[2,3]" $ do
      english "For example, if only choices 2 and 3 are cnf formulas, then the solution is:"
      german "Liegen beispielsweise nur Auswahlmöglichkeiten 2 und 3 in KNF vor, dann ist diese Lösung korrekt:"

    extra addText

    pure ()



verifyInst :: OutputCapable m => LegalCNFInst -> LangM m
verifyInst _ = pure ()



verifyConfig :: OutputCapable m => LegalCNFConfig -> LangM m
verifyConfig = checkLegalCNFConfig



start :: [Int]
start = []



partialGrade :: OutputCapable m => LegalCNFInst -> [Int] -> LangM m
partialGrade LegalCNFInst{..} = multipleChoiceSyntax False [1..length formulaStrings]


completeGrade :: OutputCapable m => LegalCNFInst -> [Int] -> Rated m
completeGrade LegalCNFInst{..} = multipleChoice DefiniteArticle what solutionDisplay (Map.fromAscList solution)
  where
    what = translations $ do
      german "Indizes"
      english "indices"
    solutionDisplay | showSolution = Just $ show [ i | (i, True) <- solution ]
                    | otherwise = Nothing
    solution = map (\i -> (i, i `notElem` serialsOfWrong)) [1..length formulaStrings]
