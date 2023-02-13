{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.TreeToFormula where


import Control.Monad.IO.Class(MonadIO (liftIO))
import Control.Monad.Output (LangM, OutputMonad(..))
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.SHA (sha1, showDigest)
import Image.LaTeX.Render (FormulaOptions(..), SVG, defaultEnv, imageForFormula)

import LogicTasks.Syntax.Helpers
import Tasks.SynTree.Config (checkSynTreeConfig, SynTreeInst(..), SynTreeConfig)
import Tasks.SynTree.Quiz (feedback)
import Trees.Types (BinOp, SynTree(..))




description :: (OutputMonad m, MonadIO m) => FilePath -> SynTreeInst -> LangM m
description path SynTreeInst{..} = do
    instruct
      "Consider the following syntax tree:"
      "Betrachten Sie den folgenden Syntaxbaum:"

    picture <- liftIO $ cacheTree latexImage path

    image picture

    instruct
      "Give the propositional logic formula represented by this syntax tree."
      "Geben Sie die aussagenlogische Formel an, die von diesem Syntaxbaum dargestellt wird."

    instruct
      "(You are allowed to add arbitrarily many additional pairs of brackets.)"
      "(Dabei dürfen Sie beliebig viele zusätzliche Klammerpaare hinzufügen.)"




verifyInst :: OutputMonad m => SynTreeInst -> LangM m
verifyInst _ = pure()



verifyConfig :: OutputMonad m => SynTreeConfig -> LangM m
verifyConfig = checkSynTreeConfig



start :: SynTree BinOp Char
start = Leaf ' '



partialGrade :: OutputMonad m => SynTreeInst -> SynTree BinOp Char -> LangM m
partialGrade _ _ = pure()



completeGrade :: OutputMonad m => SynTreeInst -> SynTree BinOp Char -> LangM m
completeGrade inst sol
    | not $ feedback inst sol = reject
      "Your solution is not correct."
      "Ihre Abgabe ist nicht die korrekte Lösung."
    | otherwise = pure()



treeOptions :: FormulaOptions
treeOptions = FormulaOptions "\\usepackage[linguistics]{forest}" Nothing



getImage :: String -> IO SVG
getImage s = do
  let iTree = "\\begin{forest}" ++ s ++ "\\end{forest}"
  render <- imageForFormula defaultEnv treeOptions iTree
  case render of (Left _) -> error "failed to render an image with the given formula."
                 (Right svg) -> pure svg




outputImage :: FilePath -> String -> IO FilePath
outputImage path tree = do
  img <- getImage tree
  writeFile path img
  pure path



cacheTree :: String -> FilePath -> IO FilePath
cacheTree tree path = cacheIO path ext "tree" tree outputImage
  where ext = showDigest (sha1 . fromString $ tree) ++ ".svg"
