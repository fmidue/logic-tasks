{-# language RecordWildCards #-}

module LogicTasks.Semantics.Min where


import Control.Monad.Output (LangM, OutputMonad (..), english, german, translate)
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Test.QuickCheck (Gen)

import Config (BaseConfig(..), CnfConfig(..), MinMaxConfig(..), MinInst(..))
import Formula.Table (readEntries)
import Formula.Types (Dnf, Literal(..), amount, atomics, genDnf, getConjunctions, getTable)
import Formula.Util (mkCon, mkDnf, hasEmptyCon, isEmptyDnf)
import Util (checkCnfConf, isOutside, pairwiseCheck, prevent, preventWithHint, tryGen, withRatio)




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

  paragraph $ translate $ do
    german "Geben Sie eine zu der Tafel passende Formel in disjunktiver Normalform an. Verwenden Sie dazu Min-Terme."
    english "Provide a formula in disjunctive normal form, that corresponds to the table. Use minterms to do this."

  paragraph $ translate $ do
    german "Reichen Sie ihre Lösung als ascii-basierte Formel ein."
    english "Provide the solution as an ascii based formula."

  paragraph $ translate $ do
    german "Beachten Sie dabei die folgende Legende:"
    english "Use the following key:"

  paragraph $ indent $ do
    text "Negation:"
    code "~"

  paragraph $ indent $ do
    translate $ do
      german "Oder:"
      english "Or:"
    code "\\/"

  paragraph $ indent $ do
    translate $ do
      german "Und:"
      english "And:"
    code "/\\"

  paragraph $ indent $ do
    translate $ do
      german "Ein Lösungsversuch könnte beispielsweise so aussehen: "
      english "A valid solution could look like this: "
    code $ show $ mkDnf [mkCon [Literal 'A', Not 'B'], mkCon [Not 'C', Not 'D']]

  paragraph $ text (fromMaybe "" addText)



verifyStatic :: OutputMonad m => MinInst -> LangM m
verifyStatic MinInst{..}
    | isEmptyDnf dnf || hasEmptyCon dnf =
        refuse $ indent $ translate $ do
          german "Geben Sie bitte eine nicht-leere Formel an."
          english "Please give a non empty formula."

    | otherwise = pure ()



verifyQuiz :: OutputMonad m => MinMaxConfig -> LangM m
verifyQuiz MinMaxConfig{..}


    | isOutside 0 100 low || isOutside 0 100 high =
        refuse $ indent $ translate $ do
          german "Die Beschränkung der Wahr-Einträge liegt nicht zwischen 0 und 100 Prozent."
          english "The given restriction on true entries are not in the range of 0 to 100 percent."

    | low > high =
        refuse $ indent $ translate $ do
          german "Die Beschränkung der Wahr-Einträge liefert keine gültige Reichweite."
          english "The given restriction on true entries are not a valid range."

    | otherwise = checkCnfConf cnfConf
  where
    (low,high) = fromMaybe (0,100) percentTrueEntries



start :: Dnf
start = mkDnf []



partialGrade :: OutputMonad m => MinInst -> Dnf -> LangM m
partialGrade MinInst{..} sol = do
  preventWithHint (not $ null extra)
    (translate $ do
      german "Angegebene Literale kommen in Aufgabe vor?"
      english "Given literals are used in task?"
    )

    (paragraph $ do
      translate $ do
        german "Es sind unbekannte Literale enthalten. Diese Literale kommen in der korrekten Lösung nicht vor: "
        english "Your submission contains unknown literals. These do not appear in a correct solution: "
      itemizeM $ map (text . show) extra
    )

  preventWithHint (not $ null missing)
    (translate $ do
      german "Alle Literale kommen vor?"
      english "All literals are contained in solution?"
    )

    (paragraph $ do
      translate $ do
        german "Es fehlen Literale. Fügen Sie Diese Literale der Abgabe hinzu: "
        english "Some literals are missing. Add these literals to your submission: "
      itemizeM $ map (text . show) missing
    )

  prevent (not $ all (\c -> amount c == length corLits) $ getConjunctions sol) $
    translate $ do
      german "Alle Konjunktionen sind Minterme?"
      english "All conjunctions are minterms?"

  preventWithHint (solLen < corrLen)
    (translate $ do
      german "Genügend Maxterme in Lösung?"
      english "Solution contains enough maxterms?"
    )

    (paragraph $ do
      translate $ do
        german "Die angegebene Formel enthält zu wenige Minterme. Fügen sie "
        english "The formula does not contain enough minterms. Add "
      text diff
      translate $ do
        german " hinzu!"
        english "!"
    )

  preventWithHint (solLen > corrLen)
    (translate $ do
      german "Nicht zu viele Minterme in Lösung?"
      english "Not too many minterms in solution?"
    )

    (paragraph $ do
      translate $ do
        german " Die angegebene Formel enthält zu viele Minterme. Entfernen sie "
        english "The formula contains too many minterms. Remove "
      text $ diff ++ "!"
    )
  where
    solLits = atomics sol
    corLits = atomics dnf
    extra = solLits \\ corLits
    missing = corLits \\ solLits
    table = getTable dnf
    corrLen = length $ filter (== Just True) (readEntries table)
    solLen = amount sol
    diff = show $ abs (solLen - corrLen)



completeGrade :: OutputMonad m => MinInst -> Dnf -> LangM m
completeGrade MinInst{..} sol = do
  preventWithHint (not $ null diff)
    (translate $ do
      german "Lösung liefert korrekte Wahrheitstabelle?"
      english "Solution gives correct truth table?"
    )
    (paragraph $ do
      translate $ do
        german "Es existieren falsche Einträge in den folgenden Tabellenspalten: "
        english "The following rows are not correct: "
      itemizeM $ map (text . show) diff
    )
  where
    solTable = getTable sol
    (_,diff) = pairwiseCheck (zip3 (readEntries solTable) (readEntries $ getTable dnf) [1..])
