{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.IllegalFormulas where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (code, image),
  LangM,
  OutputCapable,
  ($=<<),
  english,
  german,
  Rated,
  multipleChoice,
  ArticleToUse (DefiniteArticle),
  translations,
  )
import Data.List (nub, sort)
import LogicTasks.Helpers (example, extra, focus, indexed, instruct, reject, reRefuse)
import Tasks.LegalProposition.Config (LegalPropositionInst(..), LegalPropositionConfig(..), checkLegalPropositionConfig)
import Control.Monad (when)
import Trees.Print (transferToPicture)
import Control.Monad.IO.Class (MonadIO (liftIO))
import LogicTasks.Syntax.TreeToFormula (cacheTree)
import Data.Foldable (for_)
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as Map (fromAscList)
import Control.Applicative (Alternative)




description :: OutputCapable m => LegalPropositionInst -> LangM m
description LegalPropositionInst{..} = do
    instruct $ do
      english "Consider the following propositional (pseudo) formulas:"
      german "Betrachten Sie die folgenden aussagenlogischen (Pseudo-)Formeln:"

    focus $ unlines $ indexed $ map fst pseudoFormulas

    instruct $ do
      english "Some of these are syntactically wrong. Which of these formulas are correctly formed?"
      german "Einige davon enthalten syntaktische Fehler. Geben Sie an, welche Formeln korrekt geformt sind."

    instruct $ do
      english "Enter a list containing the indices of the syntactically correct formulas to submit your answer."
      german "Geben Sie eine Liste der Indizes aller syntaktisch korrekten Formeln als Ihre Lösung an."

    example "[2,3]" $ do
      english "For example, if only choices 2 and 3 are correctly formed, then the solution is:"
      german "Sind beispielsweise nur Auswahlmöglichkeiten 2 und 3 richtig geformt, dann ist diese Lösung korrekt:"

    extra addText
    pure ()


verifyInst :: OutputCapable m => LegalPropositionInst -> LangM m
verifyInst _ = pure ()



verifyConfig :: OutputCapable m => LegalPropositionConfig -> LangM m
verifyConfig = checkLegalPropositionConfig



start :: [Int]
start = []



partialGrade :: OutputCapable m => LegalPropositionInst -> [Int] -> LangM m
partialGrade LegalPropositionInst{..} sol
    | invalidIndex = reject $ do
      english "At least one index in the list does not exist."
      german "Mindestens einer der Indizes existiert nicht."

    | otherwise = pure ()
  where
    nubSol = nub sol
    invalidIndex = any (`notElem` [1..length pseudoFormulas]) nubSol



completeGrade
  :: (OutputCapable m, MonadIO m, Alternative m)
  => FilePath
  -> LegalPropositionInst
  -> [Int]
  -> Rated m
completeGrade path LegalPropositionInst{..} sol = reRefuse
    (multipleChoice DefiniteArticle what solutionDisplay solution sol)
    $ when (showSolution && wrongSolution) $ do
      instruct $ do
          english "The following syntax trees represent the well-formed formulas:"
          german "Die folgenden Syntaxbäume entsprechen den wohlgeformten Formeln:"

      for_ correctTrees $ \(i,pf,t) -> do
        code $ show i ++ ". " ++ pf
        image $=<< liftIO $ cacheTree (transferToPicture t) path
        pure ()

      pure ()


    where
      wrongSolution = sort (nub sol) /= sort serialsOfRight
      pseudoIndexed = zip ([1..] :: [Int]) pseudoFormulas
      correctEntries = filter (\(_,(_,mt)) -> isJust mt) pseudoIndexed
      serialsOfRight = map fst correctEntries
      correctTrees = map (\(i,(pf,t)) -> (i,pf,fromJust t)) correctEntries
      what = translations $ do
        german "Indizes"
        english "indices"
      solutionDisplay | showSolution = Just $ show serialsOfRight
                      | otherwise = Nothing
      solution = Map.fromAscList $ map (\(i,(_,mt)) -> (i, isJust mt)) pseudoIndexed

