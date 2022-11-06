{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.SubTreeSet where


import Control.Monad.Output (LangM, OutputMonad(..), english, german, translate)
import Tasks.SubTree.Config (checkSubTreeConfig, SubTreeInst(..), SubTreeConfig(..))




description :: OutputMonad m => SubTreeInst -> LangM m
description SubTreeInst{..} = do
    paragraph $ translate $ do
      german "Betrachten Sie die folgende aussagenlogische Formel:"
      english "Consider the following propositional logic formula:"

    indent $ code $ formula

    paragraph $ translate $ do
      german $ "Finden Sie " ++ show minInputTrees ++ " nicht atomare Subformeln, die in dieser Formel enthalten sind."
      english $ "Find " ++ show minInputTrees ++ " non atomic subformulae that are contained in it."

    paragraph $ translate $ do
      german "Geben Sie die Lösung als eine Liste der Subformeln an."
      english "Submit your solution as a  list of subformulae."

    paragraph $ translate $ do
      german "Entfernen Sie dabei Klammerpaare, die eine Subformel komplett umschließen und fügen Sie keine zusätzlichen Klammern hinzu."
      english "Remove bracket pairs which only serve to enclose entire Subformulae and do not add any additional brackets."

    paragraph $ translate $ do
      german "Ist z.B. ~(A \\/ B) die gegebene Formel und es wird eine Subformel gesucht, dann ist die folgende Lösung korrekt:"
      english "For example, if ~(A \\/ B) is the given formula and one subformula is required, then the solution is:"

    indent $ code "A \\/ B"




verifyInst :: OutputMonad m => SubTreeInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputMonad m => SubTreeConfig -> LangM m
verifyConfig = checkSubTreeConfig



start :: [String]
start = []



partialGrade :: OutputMonad m => SubTreeInst -> [String] -> LangM m
partialGrade _ _ = pure()



completeGrade :: OutputMonad m => SubTreeInst -> [String] -> LangM m
completeGrade _ _ = pure()
