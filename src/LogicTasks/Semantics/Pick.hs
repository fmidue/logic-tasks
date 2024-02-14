{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}

module LogicTasks.Semantics.Pick where


import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  OutputMonad,
  english,
  german,
  translate,
  )

import Test.QuickCheck (Gen, elements, vectorOf)

import Config (BaseConfig(..), CnfConfig(..), Number(..), PickConfig(..), PickInst(..))
import Formula.Util (mkCnf, isSemanticEqual)
import Formula.Types (availableLetter, genCnf, getTable, literals)
import Formula.Printing (showIndexedList)
import Util (checkCnfConf, tryGen)
import LogicTasks.Helpers (example, extra)
import Control.Monad (when)
import Data.Maybe (fromJust)
import Data.List (nubBy)




genPickInst :: PickConfig -> Gen PickInst
genPickInst PickConfig{ cnfConf = CnfConfig {baseConf = BaseConfig{..}, ..}, ..} = do
    cnfs <- tryGen (vectorOf amountOfOptions (getCnf usedLiterals)) 100 ((amountOfOptions ==) . length . nubBy isSemanticEqual)
    corrIndex <- elements [1..amountOfOptions]
    pure $ PickInst cnfs corrIndex printSolution extraText
  where
    getCnf = genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength)



description :: OutputMonad m => PickInst -> LangM m
description PickInst{..} = do
    paragraph $ do
      translate $ do
        german "Betrachten Sie die folgende Formel:"
        english "Consider the following formula:"
      indent $ code $ availableLetter (literals sTable) : " = " ++ show sTable
      pure ()
    paragraph $ do
      translate $ do
        german "Welche der folgenden Wahrheitstafeln passt zu der Formel? Geben Sie die richtige Tafel durch ihre Nummer an."
        english "Which of these truth tables represents the formula? Specify the correct table by giving its number."
      indent $ code $ showIndexedList 120 5 $ map getTable cnfs
      pure ()
    paragraph $ indent $ do
      translate $ do
        german "Ein Lösungsversuch könnte beispielsweise so aussehen: "
        english "A valid solution could look like this: "
      code "1"
      pure ()
    extra addText
    pure ()
  where
    sTable = cnfs !! (correct - 1)


verifyStatic :: OutputMonad m => PickInst -> LangM m
verifyStatic PickInst{..}
    | null cnfs =
        refuse $ indent $ translate $ do
          german "Die Liste der Formeln ist leer."
          english "The list of formulas is empty."

    | mkCnf [] `elem` cnfs =
        refuse $ indent $ translate $ do
          german "Für mindestens eine Formel kann keine Wahrheitstafel erstellt werden."
          english "For at least one given formula there is no corresponding truth table."

    | length cnfs < correct || correct <= 0 =
        refuse $ indent $ translate $ do
          german "Der angegebene Index existiert nicht."
          english "The given index does not exist."

    | otherwise = pure()



verifyQuiz :: OutputMonad m => PickConfig -> LangM m
verifyQuiz PickConfig{..}

    | amountOfOptions < 2 =
        refuse $ indent $ translate $ do
          german "Es muss mindestens zwei Optionen geben."
          english "At least two options need to be given."

    | amountOfOptions > 4*2^ length (usedLiterals base) =
        refuse $ indent $ translate $ do
          german "Die Anzahl Optionen übersteigt die Anzahl möglicher, unterschiedlicher Formeln."
          english "The amount of options is higher than the amount of possible, distinct formulas."

    | otherwise = checkCnfConf cnfConf
  where
    base = baseConf cnfConf



start :: Number
start = Number Nothing



partialGrade :: OutputMonad m => PickInst -> Number -> LangM m
partialGrade _ (Number Nothing) = refuse $ indent $
        translate $ do
          german "Es wurde kein Index angegeben."
          english "You did not give an index."

partialGrade _ _ = pure ()


completeGrade :: OutputMonad m => PickInst -> Number -> LangM m
completeGrade PickInst{..} (Number index) =
    if fromJust index == correct
        then pure ()
        else refuse $ indent $ do
          translate $ do
            german "Der gewählte Index ist falsch."
            english "You submitted the wrong index."

          displaySolution

          pure ()
  where displaySolution = when showSolution $ example (show correct) $ do
          english "The solution for this task is:"
          german "Die Lösung für die Aufgabe ist:"
