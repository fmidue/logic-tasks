{-# LANGUAGE RecordWildCards #-}
module LogicTasks.Syntax.IllegalCnfs where


import Control.Monad.Output (LangM, OutputMonad (..), english, german, translate)
import Tasks.LegalCNF.Config(LegalCNFConfig(..), LegalCNFInst(..), checkLegalCNFConfig)




description :: OutputMonad m => LegalCNFInst -> LangM m
description LegalCNFInst{..} = do
    paragraph $ translate $ do
      german "Betrachten Sie die folgenden aussagenlogischen Formeln:"
      english "Consider the following propositional logic formulae:"

    indent $ code $ unlines formulaStrings

    paragraph $ translate $ do
      german "Welche dieser Formeln sind nicht in konjunktiver Normalform (cnf) angegeben?"
      english "Which of these formulae are not given in conjunctive normal form (cnf)?"

    paragraph $ translate $ do
      german "Geben Sie eine Liste der Indices aller nicht cnf-Formeln als Ihre Lösung an."
      english "Enter a list containing the indices of the non-cnf formulae to submit your answer."

    paragraph $ translate $ do
      german "Sind beispielsweise nur Auswahlmöglichkeiten 2 und 3 keine cnf-Formeln, dann ist diese Lösung korrekt:"
      english "For example, if only choices 2 and 3 are non-cnf formulae, then the solution is:"

    indent $ code "[2,3]"




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

