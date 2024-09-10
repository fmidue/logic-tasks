{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.IllegalFormulas where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (refuse, code, image),
  LangM,
  OutputCapable,
  ($=<<),
  english,
  german,
  )
import Data.List (nub, sort)

import LogicTasks.Helpers (example, extra, focus, indexed, instruct, reject)
import Tasks.LegalProposition.Config (LegalPropositionInst(..), LegalPropositionConfig(..), checkLegalPropositionConfig)
import Control.Monad (when)
import Trees.Print (transferToPicture)
import Control.Monad.IO.Class (MonadIO (liftIO))
import LogicTasks.Syntax.TreeToFormula (cacheTree)
import Data.Foldable (for_)
import Data.Maybe (isNothing, isJust, fromJust)




description :: OutputCapable m => LegalPropositionInst -> LangM m
description LegalPropositionInst{..} = do
    instruct $ do
      english "Consider the following propositional (pseudo) formulas:"
      german "Betrachten Sie die folgenden aussagenlogischen (Pseudo-)Formeln:"

    focus $ unlines $ indexed $ map fst pseudoFormulas

    instruct $ do
      english "Some of these are syntactically incorrect. Which of these formulas have an invalid format?"
      german "Einige davon enthalten syntaktische Fehler. Geben Sie an, welche Formeln nicht korrekt geformt sind."

    instruct $ do
      english "Enter a list containing the indices of the invalid formulas to submit your answer."
      german "Geben Sie eine Liste der Indizes aller syntaktisch falschen Formeln als Ihre Lösung an."

    example "[2,3]" $ do
      english "For example, if only choices 2 and 3 are incorrect, then the solution is:"
      german "Sind beispielsweise nur Auswahlmöglichkeiten 2 und 3 falsch, dann ist diese Lösung korrekt:"

    extra addText
    pure ()


verifyInst :: OutputCapable m => LegalPropositionInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputCapable m => LegalPropositionConfig -> LangM m
verifyConfig = checkLegalPropositionConfig



start :: [Int]
start = []



partialGrade :: OutputCapable m => LegalPropositionInst -> [Int] -> LangM m
partialGrade LegalPropositionInst{..} sol
    | invalidIndex = reject $ do
      english "At least one index in the list does not exist."
      german "Mindestens einer der Indizes existiert nicht."

    | otherwise = pure()
  where
    nubSol = nub sol
    invalidIndex = any (`notElem` [1..length pseudoFormulas]) nubSol



completeGrade
  :: (OutputCapable m, MonadIO m)
  => FilePath
  -> LegalPropositionInst
  -> [Int]
  -> LangM m
completeGrade path inst sol = refuseIfWrong $ do
  when wrongSolution $ do
     instruct $ do
        english "Your solution is incorrect."
        german "Ihre Lösung ist falsch."

  when (showSolution inst) $ do
    when wrongSolution $
      example (show serialsOfWrong) $ do
          english "A possible solution for this task is:"
          german "Eine mögliche Lösung für die Aufgabe ist:"

    instruct $ do
        english "The following syntax trees represent the well-formed formulas:"
        german "Die folgenden Syntaxbäume entsprechen den wohlgeformten Formeln:"

    for_ correctTrees $ \(i,pf,t) -> do
      code $ show i ++ ". " ++ pf
      image $=<< liftIO $ cacheTree (transferToPicture t) path
      pure ()

    pure ()

  pure ()
  where
    wrongSolution = sort (nub sol) /= sort serialsOfWrong
    refuseIfWrong = if wrongSolution then refuse else id
    pseudoIndexed = zip [1..] (pseudoFormulas inst)
    serialsOfWrong = map fst $ filter (\(_,(_,mt)) -> isNothing mt) pseudoIndexed
    correctTrees = map (\(i,(pf,t)) -> (i,pf,fromJust t)) $ filter (\(_,(_,mt)) -> isJust mt) pseudoIndexed
