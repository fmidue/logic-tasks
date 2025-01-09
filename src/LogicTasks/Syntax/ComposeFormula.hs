{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module LogicTasks.Syntax.ComposeFormula where


import Control.Monad.IO.Class(MonadIO (liftIO))
import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  OutputCapable,
  ($=<<),
  english,
  german, translate, localise, translations,
  )
import Data.Maybe (fromJust, isNothing)

import LogicTasks.Helpers (extra, instruct, keyHeading, reject, example, basicOpKey, arrowsKey)
import Trees.Types (TreeFormulaAnswer(..), SynTree (Binary), showOperator)
import Control.Monad (when)
import Trees.Print (transferToPicture, display)
import Tasks.ComposeFormula.Config (ComposeFormulaInst(..), ComposeFormulaConfig, checkComposeFormulaConfig)
import Trees.Helpers (collectLeaves, collectUniqueBinOpsInSynTree)
import Data.Containers.ListUtils (nubOrd)
import LogicTasks.Syntax.TreeToFormula (cacheTree)
import Data.Foldable (for_)
import Formula.Parsing (Parse(parser), formulaListSymbolParser)
import Formula.Parsing.Delayed (Delayed, withDelayedSucceeding, parseDelayedWithAndThen, complainAboutMissingParenthesesIfNotFailingOn)




description :: (OutputCapable m, MonadIO m) => Bool -> FilePath -> ComposeFormulaInst -> LangM m
description inputHelp path ComposeFormulaInst{..} = do
    instruct $ do
      english $ "Imagine that the two displayed " ++ eTreesOrFormulas ++ " are hung below a root node with operator "
      english $ showOperator operator
      english $ ". Once one " ++ eTreeOrFormula ++" on the left and the other " ++ eTreeOrFormula ++" on the right, and once the other way around."
      german $ "Stellen Sie sich vor, die beiden angezeigten " ++ gTreesOrFormulas ++" würden unterhalb eines Wurzelknotens mit Operator "
      german $ showOperator operator
      german $ " gehängt. Einmal " ++ derDie ++" eine " ++ gTreeOrFormula ++" links und " ++ derDie ++" andere " ++ gTreeOrFormula ++" rechts, und einmal genau andersherum."

    instruct $ do
      english $ "This is the first " ++ eTreeOrFormula ++ ":"
      german $ "Dies ist " ++ derDie ++ " erste " ++ gTreeOrFormula ++ ":"

    case leftTreeImage of
      Nothing -> paragraph $ code $ display leftTree
      Just image' -> image $=<< liftIO $ cacheTree image' path

    instruct $ do
      english $ "This is the second " ++ eTreeOrFormula ++ ":"
      german $ "Dies ist " ++ derDie ++ " zweite " ++ gTreeOrFormula ++ ":"

    case rightTreeImage of
      Nothing -> paragraph $ code $ display rightTree
      Just image' -> image $=<< liftIO $ cacheTree image' path

    instruct $ do
      english $ "Build the corresponding formulas for the two resulting trees" ++ onListsEng ++ ". "
      english $ "The order of the formulas" ++ onOrderEng ++ "does not matter."
      german $ "Bilden Sie für die beiden entstehenden Bäume die repräsentierenden Formeln" ++ onListsGer ++ ". "
      german $ "Es spielt keine Rolle, in welcher Reihenfolge die Formeln " ++ onOrderGer ++ "."

    instruct $ do
      english "(You are allowed to add arbitrarily many additional pairs of brackets in the formulas.)"
      german "(In den Formeln dürfen Sie beliebig viele zusätzliche Klammerpaare hinzufügen.)"

    when addExtraHintsOnAssociativity $ instruct $ do
        english "Remark: Do not try to use associativity in order to omit brackets in this task."
        german "Hinweis: Sie dürfen bei dieser Aufgabe nicht Klammern durch Verwendung von Assoziativität weglassen."

    keyHeading
    basicOpKey unicodeAllowed
    arrowsKey

    when inputHelp $ paragraph $ indent $ do
      translate $ do
        english "A solution attempt could look like this: "
        german "Ein Lösungsversuch könnte beispielsweise so aussehen: "
      translatedCode $ flip localise $ translations exampleCode
      pure ()

    extra addText
    pure ()
      where
        derDie = derDie' leftTreeImage rightTreeImage
        derDie' Nothing Nothing = "die"
        derDie' (Just _) (Just _) = "der"
        derDie' _ _ = "der/die"
        (gTreeOrFormula, eTreeOrFormula) = treeOrFormula leftTreeImage rightTreeImage
        treeOrFormula Nothing Nothing = ("Formel", "formula")
        treeOrFormula (Just _) (Just _) = ("Baum", "tree")
        treeOrFormula _ _ = ("Baum/Formel", "tree/formula")
        (gTreesOrFormulas, eTreesOrFormulas) = treesOrFormulas leftTreeImage rightTreeImage
        treesOrFormulas Nothing Nothing = ("Formeln", "formulas") -- no-spell-check
        treesOrFormulas (Just _) (Just _) = ("Bäume", "trees")
        treesOrFormulas _ _ = ("Bäume/Formeln", "trees/formulas") -- no-spell-check
        exampleCode | unicodeAllowed = do
                      english "[(A ∨ ¬B) and C, C and (A or not B)]"
                      german "[(A ∨ ¬B) und C, C und (A oder nicht B)]"
                    | otherwise      = do
                      english "[(A or not B) and C, C and (A or not B)]"
                      german "[(A oder nicht B) und C, C und (A oder nicht B)]"
        (onListsEng, onListsGer)
          | inputHelp = (" and put them into a list", " und geben Sie diese in einer Liste an") -- no-spell-check
          | otherwise = ("", "")
        (onOrderEng,onOrderGer)
          | inputHelp = (" in the list ", "in der Liste stehen") -- no-spell-check
          | otherwise = (" ", "angegeben werden") -- no-spell-check


verifyInst :: OutputCapable m => ComposeFormulaInst -> LangM m
verifyInst _ = pure ()



verifyConfig :: OutputCapable m => ComposeFormulaConfig -> LangM m
verifyConfig = checkComposeFormulaConfig



start :: [TreeFormulaAnswer]
start = []


partialGrade :: OutputCapable m => ComposeFormulaInst -> Delayed [TreeFormulaAnswer] -> LangM m
partialGrade = parseDelayedWithAndThen parser complainAboutMissingParenthesesIfNotFailingOn formulaListSymbolParser . partialGrade'

partialGrade' :: OutputCapable m => ComposeFormulaInst -> [TreeFormulaAnswer] -> LangM m
partialGrade' ComposeFormulaInst{..} sol
  | length (nubOrd sol) /= 2 =
    reject $ do
      english "Your submission does not contain the right amount of unique formulas. There need to be exactly two different formulas."
      german  "Sie haben nicht die richtige Anzahl an einzigartigen Formeln eingegeben. Es werden genau zwei unterschiedliche Formeln erwartet."
  | any (isNothing . maybeTree) sol =
    reject $ do
      english "At least one input does not represent a syntax tree."
      german "Mindestens eine der Eingaben entspricht nicht einem Syntaxbaum."
  | not (all containsOperator parsedSol) =
    reject $ do
      english "At least one of your formulas does not contain the given operator."
      german "Mindestens eine Ihrer Formeln beinhaltet nicht den vorgegebenen Operator."
  | any (`notElem` correctLits) literals =
    reject $ do
      english "Your submission contains unknown atomic formulas."
      german "Ihre Abgabe beinhaltet unbekannte atomare Formeln."
  | any (`notElem` literals) correctLits =
    reject $ do
      english "Your submission does not contain all atomic formulas present in the original syntax trees/formulas."
      german "Ihre Abgabe beinhaltet nicht alle atomaren Formeln aus den ursprünglichen Syntaxbäumen/Formeln."
  | usedOperators > correctOperators =
    reject $ do
      english "Your submission contains too many different operators."
      german "Ihre Abgabe beinhaltet zu viele unterschiedliche Operatoren."
  | any (\s -> s == leftTree || s == rightTree) parsedSol =
    reject $ do
      english $ "At least one input corresponds to one of the " ++ eTreesOrFormulas ++ " already given. "
      english "Read the task again more carefully."
      german $ "Mindestens eine der Eingaben entspricht " ++ einerEinem ++ " der bereits gegebenen " ++ gTreesOrFormulas ++ ". "
      german "Lesen Sie die Aufgabenstellung noch einmal genauer."


  | otherwise = pure ()
    where
      parsedSol = map pForm sol
      containsOperator = (operator `elem`) . collectUniqueBinOpsInSynTree
      correctLits = nubOrd $ collectLeaves leftTree ++ collectLeaves rightTree
      literals = nubOrd $ concatMap collectLeaves parsedSol
      pForm = fromJust . maybeTree
      usedOperators = length $ nubOrd $ operator : concatMap (collectUniqueBinOpsInSynTree . pForm) sol
      correctOperators = length $ nubOrd $
        collectUniqueBinOpsInSynTree leftTree ++
          collectUniqueBinOpsInSynTree rightTree ++ [operator]

      einerEinem = einerEinem' leftTreeImage rightTreeImage
      einerEinem' Nothing Nothing = "einer"
      einerEinem' (Just _) (Just _) = "einem"
      einerEinem' _ _ = "einem/einer"
      (gTreesOrFormulas, eTreesOrFormulas) = treesOrFormulas leftTreeImage rightTreeImage
      treesOrFormulas Nothing Nothing = ("Formeln", "formulas") -- no-spell-check
      treesOrFormulas (Just _) (Just _) = ("Bäume", "trees")
      treesOrFormulas _ _ = ("Bäume/Formeln", "trees/formulas") -- no-spell-check

completeGrade :: (OutputCapable m, MonadIO m) =>
  FilePath -> ComposeFormulaInst -> Delayed [TreeFormulaAnswer] -> LangM m
completeGrade path inst = completeGrade' path inst `withDelayedSucceeding` parser

completeGrade' :: (OutputCapable m, MonadIO m) =>
  FilePath -> ComposeFormulaInst -> [TreeFormulaAnswer] -> LangM m
completeGrade' path ComposeFormulaInst{..} sol
  | lrTree `notElem` parsedSol || rlTree `notElem` parsedSol = refuse $ do
    instruct $ do
      english "Your submission is not correct. The syntax trees for your entered formulas look like this:"
      german "Ihre Abgabe ist nicht die korrekte Lösung. Die Syntaxbäume zu Ihren eingegebenen Formeln sehen so aus:"

    for_ parsedSol $ \synTree ->
      image $=<< liftIO $ cacheTree (transferToPicture synTree) path

    when showSolution $
      example (concat ["[", display lrTree, ",", display rlTree, "]"]) $ do
        english "A possible solution for this task is:"
        german "Eine mögliche Lösung für diese Aufgabe ist:"

    pure ()
  | otherwise = pure ()
    where
      parsedSol = map (fromJust . maybeTree) sol
      lrTree = Binary operator leftTree rightTree
      rlTree = Binary operator rightTree leftTree
