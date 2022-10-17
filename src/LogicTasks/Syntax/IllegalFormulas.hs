{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.IllegalFormulas where


import Control.Monad.Output (LangM, OutputMonad(..), english, german, translate)
import Tasks.LegalProposition.Config (LegalPropositionInst(..), LegalPropositionConfig(..), checkLegalPropositionConfig)




description :: OutputMonad m => LegalPropositionInst -> LangM m
description LegalPropositionInst{..} = do
    paragraph $ translate $ do
      german "Betrachten Sie die folgenden aussagenlogischen Formeln:"
      english "Consider the following propositional formulae:"

    indent $ code $ unlines pseudoFormulas

    paragraph $ translate $ do
      german "Einige dieser Formeln enthalten syntaktische Fehler. Geben Sie an, welche Formeln nicht korrekt sind."
      english "Some of these formulae are syntactically incorrect. Which of these formulae are invalid?"

    paragraph $ translate $ do
      german "Geben Sie eine Liste der Indices aller syntaktisch falschen Formeln als Ihre Lösung an."
      english "Enter a list containing the indices of the invalid formulae to submit your answer."

    paragraph $ translate $ do
      german "Sind beispielsweise nur Auswahlmöglichkeiten 2 und 3 falsch, dann ist diese Lösung korrekt:"
      english "For example, if only choices 2 and 3 are incorrect, then the solution is:"

    indent $ code "[2,3]"




verifyInst :: OutputMonad m => LegalPropositionInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputMonad m => LegalPropositionConfig -> LangM m
verifyConfig = checkLegalPropositionConfig

start :: [Int]
start = []



partialGrade :: OutputMonad m => LegalPropositionInst -> [Int] -> LangM m
partialGrade _ _ = pure()



completeGrade :: OutputMonad m => LegalPropositionInst -> [Int] -> LangM m
completeGrade _ _ = pure()
