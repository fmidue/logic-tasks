{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module LogicTasks.Syntax.TreeToFormula where


import Control.Monad.Output (LangM, OutputMonad(..))

import LogicTasks.Syntax.Helpers
import Tasks.SynTree.Config (checkSynTreeConfig, SynTreeInst(..), SynTreeConfig)
import Tasks.SynTree.Quiz (feedback)
import Trees.Types (PropFormula(..))



description :: OutputMonad m => SynTreeInst -> LangM m
description SynTreeInst{..} = do
    instruct
      "Consider the following syntax tree:"
      "Betrachten Sie den folgenden Syntaxbaum:"

    indent $ latex $ latexImage

    instruct
      "Find the propositional logic formula represented by this syntax tree."
      "Geben Sie die aussagenlogische Formel an, die von diesem Syntaxbaum dargestellt wird."

    instruct
      "You are allowed to add arbitrarily many additional pairs of brackets, provided that they do not change the interpretation of the formula."
      "Dabei dürfen Sie beliebig viele zusätzliche Klammerpaare hinzufügen, solange diese die Bedeutung der Formel nicht verändern."




verifyInst :: OutputMonad m => SynTreeInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputMonad m => SynTreeConfig -> LangM m
verifyConfig = checkSynTreeConfig



start :: PropFormula
start = Atomic ' '



partialGrade :: OutputMonad m => SynTreeInst -> PropFormula -> LangM m
partialGrade _ _ = pure()



completeGrade :: OutputMonad m => SynTreeInst -> PropFormula -> LangM m
completeGrade inst sol
    | not $ feedback inst sol = reject
      "Your solution is not correct."
      "Ihre Abgabe ist nicht die korrekte Lösung"
    | otherwise = pure()
