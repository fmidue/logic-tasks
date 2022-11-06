{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module LogicTasks.Syntax.SimplestFormula where


import Control.Monad.Output (LangM, OutputMonad(..), english, german, translate)
import Tasks.SuperfluousBrackets.Config (checkSuperfluousBracketsConfig, SuperfluousBracketsConfig(..), SuperfluousBracketsInst(..))




description :: OutputMonad m => SuperfluousBracketsInst -> LangM m
description SuperfluousBracketsInst{..} = do
    paragraph $ translate $ do
      german "Betrachten Sie die folgende aussagenlogische Formel:"
      english "Consider the following propositional logic formula:"

    indent $ code $ stringWithSuperfluousBrackets

    paragraph $ translate $ do
      german "Aufgrund der Assoziativität von /\\ und \\/ müssen Formeln mit drei oder mehr atomaren Aussagen und den gleichen logischen Operatoren nicht geklammert werden, z.B. bei:"
      english "Since /\\ and \\/ are associative, it is not necessary to use brackets when combining three or more atoms with the same operator, for example in:"

    indent $ code $ "A/\\B/\\C"

    paragraph $ translate $ do
      german "Entfernen Sie alle unnötigen Klammer-Paare in der gegebenen Formel. Geben Sie die Lösung in Form einer Aussagenlogischen Formel an."
      english "Remove all unnecessary pairs of brackets in the given formula. Give your answer as a propositional formula."

    paragraph $ translate $ do
      german "Ist z.B. (A \\/ B) die gegebene Formel, dann ist die folgende Lösung korrekt:"
      english "For example, if (A \\/ B) is the given formula, then the solution is:"

    indent $ code "A \\/ B"




verifyInst :: OutputMonad m => SuperfluousBracketsInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputMonad m => SuperfluousBracketsConfig -> LangM m
verifyConfig = checkSuperfluousBracketsConfig



start :: String
start = ""



partialGrade :: OutputMonad m => SuperfluousBracketsInst -> String -> LangM m
partialGrade _ _ = pure()



completeGrade :: OutputMonad m => SuperfluousBracketsInst -> String -> LangM m
completeGrade _ _ = pure()
