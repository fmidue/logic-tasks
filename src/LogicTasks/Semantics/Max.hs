{-# language RecordWildCards #-}

module LogicTasks.Semantics.Max where


import Control.Monad.Output (LangM, OutputMonad (..), english, german, translate)
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Test.QuickCheck (Gen)

import Config (BaseConfig(..), CnfConfig(..),  MaxInst(..), MinMaxConfig(..))
import Formula.Util (hasEmptyClause, isEmptyCnf, mkClause, mkCnf)
import Formula.Table (readEntries)
import Formula.Types (Cnf, Literal(..), amount, atomics, genCnf, getClauses, getTable)
import Util (checkCnfConf, isOutside, pairwiseCheck, prevent, preventWithHint, tryGen, withRatio)




genMaxInst :: MinMaxConfig -> Gen MaxInst
genMaxInst MinMaxConfig {cnfConf = CnfConfig {baseConf = BaseConfig{..},..},..} =
    MaxInst <$> cnfInRange <*> pure extraText
  where
    getCnf = genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals
    cnfInRange = tryGen getCnf 100 $ withRatio $ fromMaybe (0,100) percentTrueEntries



description :: OutputMonad m => MaxInst -> LangM m
description MaxInst{..} = do
  paragraph $ do
    translate $ do
      german "Betrachten Sie die folgende Wahrheitstafel:"
      english "Consider the following truth table:"
    indent $ code $ show $ getTable cnf

  paragraph $ translate $ do
    german "Geben Sie eine zu der Tafel passende Formel in konjunktiver Normalform an. Verwenden Sie dazu Max-Terme."
    english "Provide a formula in conjunctive normal form, that corresponds to the table. Use maxterms to do this."

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
    code $ show $ mkCnf [mkClause [Literal 'A', Not 'B'], mkClause [Not 'C', Not 'D']]

  paragraph $ text (fromMaybe "" addText)



verifyStatic :: OutputMonad m => MaxInst -> LangM m
verifyStatic MaxInst{..}
    | isEmptyCnf cnf || hasEmptyClause cnf =
        refuse $ indent $ translate $ do
          german "Geben Sie bitte eine nicht-leere Formel an."
          english "Please give a non empty formula."

    | otherwise = pure()



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



start :: Cnf
start = mkCnf []



partialGrade :: OutputMonad m => MaxInst -> Cnf -> LangM m
partialGrade MaxInst{..} sol = do
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

  prevent (not $ all (\c -> amount c == length corLits) $ getClauses sol) $
    translate $ do
      german "Alle Klauseln sind Maxterme?"
      english "All clauses are maxterms?"

  preventWithHint (solLen < corrLen)
    (translate $ do
      german "Genügend Maxterme in Lösung?"
      english "Solution contains enough maxterms?"
    )

    (paragraph $ do
      translate $ do
        german "Die angegebene Formel enthält zu wenige Maxterme. Fügen sie "
        english "The formula does not contain enough maxterms. Add "
      text diff
      translate $ do
        german " hinzu!"
        english "!"
    )

  preventWithHint (solLen > corrLen)
    (translate $ do
      german "Nicht zu viele Maxterme in Lösung?"
      english "Not too many maxterms in solution?"
    )

    (paragraph $ do
      translate $ do
        german " Die angegebene Formel enthält zu viele Maxterme. Entfernen sie "
        english "The formula contains too many maxterms. Remove "
      text $ diff ++ "!"
    )
  where
    solLits = atomics sol
    corLits = atomics cnf
    extra = solLits \\ corLits
    missing = corLits \\ solLits
    table = getTable cnf
    corrLen = length $ filter (== Just False) (readEntries table)
    solLen = amount sol
    diff = show $ abs (solLen - corrLen)



completeGrade :: OutputMonad m => MaxInst -> Cnf -> LangM m
completeGrade MaxInst{..} sol =
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
    (_,diff) = pairwiseCheck (zip3 (readEntries solTable) (readEntries $ getTable cnf) [1..])
