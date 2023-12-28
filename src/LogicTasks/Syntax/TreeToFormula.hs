{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.TreeToFormula where


import Control.Monad.IO.Class(MonadIO (liftIO))
import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  OutputMonad,
  ($=<<),
  english,
  german,
  )
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Data.Maybe (fromJust, isNothing, fromMaybe)
import Image.LaTeX.Render (FormulaOptions(..), SVG, defaultEnv, imageForFormula)

import LogicTasks.Helpers
import Tasks.SynTree.Config (checkSynTreeConfig, SynTreeInst(..), SynTreeConfig)
import Trees.Types (TreeFormulaAnswer(..))
import Trees.Helpers (binSynTreeToMiniSatFormula)
import Formula.Util (isSemanticEqualSat)
import Control.Monad (unless)



description :: (OutputMonad m, MonadIO m) => FilePath -> SynTreeInst -> LangM m
description path SynTreeInst{..} = do
    instruct $ do
      english "Consider the following syntax tree:"
      german "Betrachten Sie den folgenden Syntaxbaum:"

    image $=<< liftIO $ cacheTree latexImage path

    instruct $ do
      english ("Give the propositional logic formula that is " ++ (if not allowArrowOperators then "exactly " else "") ++ "represented by this syntax tree.")
      german ("Geben Sie die aussagenlogische Formel an, die " ++ (if not allowArrowOperators then "exakt " else "") ++"von diesem Syntaxbaum dargestellt wird.")

    unless allowArrowOperators $ instruct $ do
      english "Refrain from semantic changes."
      german "Sehen Sie von semantischen Umformungen ab."

    instruct $ do
      english "(You are allowed to add arbitrarily many additional pairs of brackets.)"
      german "(Dabei dürfen Sie beliebig viele zusätzliche Klammerpaare hinzufügen.)"

    keyHeading
    fullKey

    paragraph $ text (fromMaybe "" extraText)
    pure ()



verifyInst :: OutputMonad m => SynTreeInst -> LangM m
verifyInst _ = pure ()



verifyConfig :: OutputMonad m => SynTreeConfig -> LangM m
verifyConfig = checkSynTreeConfig



start :: TreeFormulaAnswer
start = TreeFormulaAnswer Nothing



partialGrade :: OutputMonad m => SynTreeInst -> TreeFormulaAnswer -> LangM m
partialGrade inst sol
    | isNothing $ maybeTree sol = reject $ do
      english "You did not submit a solution."
      german "Die Abgabe ist leer."

    | fromJust (maybeTree sol) /= tree inst && not (allowArrowOperators inst) &&
      isSemanticEqualSat (binSynTreeToMiniSatFormula (fromJust (maybeTree sol))) (binSynTreeToMiniSatFormula (tree inst)) = reject $ do
      english "Are you sure that your formula represents exactly this syntax tree and not a semantically equivalent one?"
      german "Bist du dir sicher, dass deine Formel genau diesen Syntaxbaum darstellt und nicht einen semantisch äquivalenten?"

    | otherwise = pure ()



completeGrade :: OutputMonad m => SynTreeInst -> TreeFormulaAnswer -> LangM m
completeGrade inst sol
    | fromJust ( maybeTree sol) /= tree inst = reject $ do
      english "Your solution is not correct."
      german "Ihre Abgabe ist nicht die korrekte Lösung."
    | otherwise = pure ()



treeOptions :: FormulaOptions
treeOptions = FormulaOptions "\\usepackage[linguistics]{forest}" Nothing



getImage :: String -> IO SVG
getImage s = do
  let iTree = "\\begin{forest}" ++ s ++ "\\end{forest}"
  render <- imageForFormula defaultEnv treeOptions iTree
  case render of (Left err) -> error $ unlines ["failed to render an image with the given formula: ", show err]
                 (Right svg) -> pure svg



outputImage :: FilePath -> String -> IO FilePath
outputImage path tree = do
  picture <- getImage tree
  writeFile path picture
  pure path



cacheTree :: String -> FilePath -> IO FilePath
cacheTree tree path = cacheIO path ext "tree-" tree outputImage
  where ext = showDigest (sha1 . fromString $ tree) ++ ".svg"
