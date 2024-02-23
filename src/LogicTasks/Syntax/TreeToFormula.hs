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
import Data.Maybe (fromJust, isNothing)
import Image.LaTeX.Render (FormulaOptions(..), SVG, defaultEnv, imageForFormula)

import LogicTasks.Helpers (cacheIO, extra, instruct, keyHeading, reject, example, basicOpKey, arrowsKey)
import Tasks.SynTree.Config (checkSynTreeConfig, SynTreeConfig)
import Trees.Types (TreeFormulaAnswer(..))
import Formula.Util (isSemanticEqual)
import Control.Monad (when)
import Trees.Print (transferToPicture)
import Tasks.TreeToFormula.Config (TreeToFormulaInst(..))




description :: (OutputMonad m, MonadIO m) => FilePath -> TreeToFormulaInst -> LangM m
description path TreeToFormulaInst{..} = do
    instruct $ do
      english "Consider the following syntax tree:"
      german "Betrachten Sie den folgenden Syntaxbaum:"

    image $=<< liftIO $ cacheTree latexImage path

    instruct $ do
      english "Give the propositional logic formula that is represented by this syntax tree."
      german "Geben Sie die aussagenlogische Formel an, die von diesem Syntaxbaum dargestellt wird."

    instruct $ do
      english "(You are allowed to add arbitrarily many additional pairs of brackets.)"
      german "(Dabei dürfen Sie beliebig viele zusätzliche Klammerpaare hinzufügen.)"

    when addExtraHintsOnSemanticEquivalence $ instruct $ do
      english "Remarks: The exact formula of the syntax tree must be specified. Other formulas that are semantically equivalent to this formula are incorrect solutions! You are also not allowed to use associativity in this task in order to save brackets."
      german "Hinweise: Es muss die exakte Formel des Syntaxbaums angegeben werden. Andere, selbst zu dieser Formel semantisch äquivalente Formeln sind keine korrekte Lösung! Auch dürfen Sie bei dieser Aufgabe nicht Assoziativität verwenden, um Klammern einzusparen."

    keyHeading
    basicOpKey
    when showArrowOperators arrowsKey

    extra addText
    pure ()



verifyInst :: OutputMonad m => TreeToFormulaInst -> LangM m
verifyInst _ = pure ()



verifyConfig :: OutputMonad m => SynTreeConfig -> LangM m
verifyConfig = checkSynTreeConfig



start :: TreeFormulaAnswer
start = TreeFormulaAnswer Nothing



partialGrade :: OutputMonad m => TreeToFormulaInst -> TreeFormulaAnswer -> LangM m
partialGrade _ sol
    | isNothing $ maybeTree sol = reject $ do
      english "You did not submit a solution."
      german "Die Abgabe ist leer."
    | otherwise = pure ()



completeGrade :: (OutputMonad m, MonadIO m) => FilePath -> TreeToFormulaInst -> TreeFormulaAnswer -> LangM m
completeGrade path inst sol
    | treeAnswer /= correctTree = refuse $ do
        instruct $ do
          english "Your solution is not correct. The syntax tree for your entered formula looks like this:"
          german "Ihre Abgabe ist nicht die korrekte Lösung. Der Syntaxbaum zu Ihrer eingegebenen Formel sieht so aus:"

        image $=<< liftIO $ cacheTree (transferToPicture treeAnswer) path

        when (addExtraHintsOnSemanticEquivalence inst && isSemanticEqual treeAnswer correctTree) $
          instruct $ do
            english "This syntax tree is semantically equivalent to the original one, but not identical."
            german "Dieser Syntaxbaum ist semantisch äquivalent zum ursprünglich gegebenen, aber nicht identisch."

        when (showSolution inst) $
          example (show (correct inst)) $ do
            english "A possible solution for this task is:"
            german "Eine mögliche Lösung für die Aufgabe ist:"

        pure ()
    | otherwise = pure ()
  where treeAnswer = fromJust (maybeTree sol)
        correctTree = tree inst



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
