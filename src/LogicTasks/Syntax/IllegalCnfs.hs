{-# LANGUAGE RecordWildCards #-}
module LogicTasks.Syntax.IllegalCnfs where


import Control.Monad.Output (LangM, OutputMonad (..))

import LogicTasks.Syntax.Helpers
import Tasks.LegalCNF.Config(LegalCNFConfig(..), LegalCNFInst(..), checkLegalCNFConfig)




description :: OutputMonad m => LegalCNFInst -> LangM m
description LegalCNFInst{..} = do
    instruct
      "Consider the following propositional logic formulae:"
      "Betrachten Sie die folgenden aussagenlogischen Formeln:"

    focus $ unlines $ indexed formulaStrings

    instruct
      "Which of these formulae are not given in conjunctive normal form (cnf)?"
      "Welche dieser Formeln sind nicht in konjunktiver Normalform (cnf) angegeben?"

    instruct
      "Enter a list containing the indices of the non-cnf formulae to submit your answer."
      "Geben Sie eine Liste der Indices aller nicht cnf-Formeln als Ihre Lösung an."

    instruct
      "For example, if only choices 2 and 3 are non-cnf formulae, then the solution is:"
      "Sind beispielsweise nur Auswahlmöglichkeiten 2 und 3 keine cnf-Formeln, dann ist diese Lösung korrekt:"

    focus "[2,3]"




verifyInst :: OutputMonad m => LegalCNFInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputMonad m => LegalCNFConfig -> LangM m
verifyConfig = checkLegalCNFConfig

start :: [Int]
start = []



partialGrade :: OutputMonad m => LegalCNFInst -> [Int] -> LangM m
partialGrade _ _ = pure()



completeGrade :: OutputMonad m => LegalCNFInst -> [Int] -> LangM m
completeGrade _ _ = pure()

