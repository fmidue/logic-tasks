{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}

module LogicTasks.Semantics.Min where


import qualified LogicTasks.Semantics.Max as Max

import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  OutputMonad,
  english,
  german,
  translate,
  )
import Data.Maybe (fromMaybe)
import Test.QuickCheck (Gen)

import Config (BaseConfig(..), CnfConfig(..), MinMaxConfig(..), MinInst(..))
import Formula.Types (Dnf, Literal(..), amount, atomics, genDnf, getConjunctions, getTable)
import Formula.Util (mkCon, mkDnf, hasEmptyCon, isEmptyDnf)
import LogicTasks.Helpers (cnfKey)
import Util (tryGen, withRatio)




genMinInst :: MinMaxConfig -> Gen MinInst
genMinInst MinMaxConfig {cnfConf = CnfConfig {baseConf = BaseConfig{..},..},..} =
    MinInst <$> dnfInRange <*> pure extraText
   where
     getDnf = genDnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals
     dnfInRange = tryGen getDnf 100 $ withRatio $ fromMaybe (0,100) percentTrueEntries



description :: OutputMonad m => MinInst -> LangM m
description MinInst{..} = do
  paragraph $ do
    translate $ do
      german "Betrachten Sie die folgende Wahrheitstafel:"
      english "Consider the following truth table:"
    indent $ code $ show $ getTable dnf
    pure ()
  paragraph $ translate $ do
    german "Geben Sie eine zu der Tafel passende Formel in disjunktiver Normalform an. Verwenden Sie dazu Min-Terme."
    english "Provide a formula in disjunctive normal form, that corresponds to the table. Use minterms to do this."

  paragraph $ translate $ do
    german "Reichen Sie ihre Lösung als ascii-basierte Formel ein."
    english "Provide the solution as an ascii based formula."

  cnfKey

  paragraph $ indent $ do
    translate $ do
      german "Ein Lösungsversuch könnte beispielsweise so aussehen: "
      english "A valid solution could look like this: "
    code $ show $ mkDnf [mkCon [Literal 'A', Not 'B'], mkCon [Not 'C', Not 'D']]
    pure ()
  paragraph $ text (fromMaybe "" addText)
  pure ()


verifyStatic :: OutputMonad m => MinInst -> LangM m
verifyStatic MinInst{..}
    | isEmptyDnf dnf || hasEmptyCon dnf =
        refuse $ indent $ translate $ do
          german "Geben Sie bitte eine nicht-leere Formel an."
          english "Please give a non empty formula."

    | otherwise = pure ()



verifyQuiz :: OutputMonad m => MinMaxConfig -> LangM m
verifyQuiz = Max.verifyQuiz



start :: Dnf
start = mkDnf []



partialGrade :: OutputMonad m => MinInst -> Dnf -> LangM m
partialGrade MinInst{..} sol = Max.partialMinMax corLits dnf sol allMinTerms False
  where
    corLits = atomics dnf
    allMinTerms = not $ all (\c -> amount c == length corLits) $ getConjunctions sol



completeGrade :: OutputMonad m => MinInst -> Dnf -> LangM m
completeGrade MinInst{..} = Max.completeMinMax dnf
