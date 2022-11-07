{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}

module LogicTasks.Syntax.TreeToFormula where


import Control.Monad.Output (LangM, OutputMonad(..), english, german, translate)
import Tasks.SynTree.Config (checkSynTreeConfig, SynTreeInst(..), SynTreeConfig)




description :: OutputMonad m => SynTreeInst -> LangM m
description SynTreeInst{..} = do
    paragraph $ translate $ do
      german "Betrachten Sie den folgenden Syntaxbaum:"
      english "Consider the following syntax tree:"

    indent $ latex $ latexImage

    paragraph $ translate $ do
      german "Geben Sie die aussagenlogische Formel an, die von diesem Syntaxbaum dargestellt wird."
      english "Find the propositional logic formula represented by this syntax tree."

    paragraph $ translate $ do
      german "Dabei dürfen Sie beliebig viele zusätzliche Klammerpaare hinzufügen, solange diese die Bedeutung der Formel nicht verändern."
      english "You are allowed to add arbitrarily many additional pairs of brackets, provided that they do not change the interpretation of the formula."



verifyInst :: OutputMonad m => SynTreeInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputMonad m => SynTreeConfig -> LangM m
verifyConfig = checkSynTreeConfig



start :: String
start = []



partialGrade :: OutputMonad m => SynTreeInst -> String -> LangM m
partialGrade _ _ = pure()



completeGrade :: OutputMonad m => SynTreeInst -> String -> LangM m
completeGrade _ _ = pure()
