{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# language RecordWildCards #-}

module Util where


import Control.Monad.Output (
  GenericOutputMonad(..),
  LangM,
  OutputMonad,
  english,
  german,
  translate,
  yesNo,
  )
import Control.Monad.State (put, get, lift, evalStateT)
import Control.Monad (when)
import Data.List (delete)
import Test.QuickCheck(Gen, elements)

import Config (BaseConfig(..), CnfConfig(..))
import Formula.Types (Formula, getTable)
import Formula.Table (readEntries)



prevent :: OutputMonad m => Bool -> LangM m -> LangM m
prevent b = assertion $ not b



preventWithHint :: OutputMonad m => Bool -> LangM m -> LangM m -> LangM m
preventWithHint b desc hint = do
  yesNo (not b) desc
  when b (refuse $ indent hint)
  pure ()


pairwiseCheck :: Eq a => [(a,a,Int)] -> ([Int],[Int])
pairwiseCheck [] = ([],[])
pairwiseCheck ((x,y,index):xs)
    | x == y = (index:same,diff)
    | otherwise = (same,index:diff)
  where (same,diff) = pairwiseCheck xs



isOutside :: Int -> Int -> Int -> Bool
isOutside lower upper x = x < lower || x > upper



remove :: Int -> [Int] -> Gen [Int]
remove _ [] = pure []
remove 0 xs = pure xs
remove num xs = do
    out <- elements xs
    remove (num-1) $ delete out xs



withRatio :: Formula a => (Int,Int) -> a -> Bool
withRatio (lower,upper) form =
    length trueEntries <= max upperBound (if upper == 0 then 0 else 1)
        && length trueEntries >= max (if lower == 0 then 0 else 1) lowerBound
  where
    tableEntries = readEntries (getTable form)
    trueEntries = filter (== Just True) tableEntries
    percentage :: Int -> Int
    percentage num = length tableEntries *num `div` 100
    upperBound = percentage upper
    lowerBound = percentage lower



tryGen :: Gen a -> Int -> (a -> Bool) -> Gen a
tryGen gen n b = evalStateT state 0
  where
    state = do
       runs <- get
       if runs > n
         then error "Maximum amount of tries exceeded by generator!"
         else do
           res <- lift gen
           if b res then pure res
                    else do put (runs +1)
                            state



checkTruthValueRange :: OutputMonad m => (Int,Int) -> CnfConfig -> LangM m
checkTruthValueRange (low,high) cnfConf
    | isOutside 0 100 low || isOutside 0 100 high =
        refuse $ indent $ translate $ do
          german "Die Beschränkung der Wahr-Einträge liegt nicht zwischen 0 und 100 Prozent."
          english "The given restriction on true entries are not in the range of 0 to 100 percent."

    | low > high =
        refuse $ indent $ translate $ do
          german "Die Beschränkung der Wahr-Einträge liefert keine gültige Reichweite."
          english "The given restriction on true entries are not a valid range."

    | otherwise = checkCnfConf cnfConf



checkBaseConf :: OutputMonad m => BaseConfig -> LangM m
checkBaseConf BaseConfig{..}
    | any (<1) [minClauseLength, maxClauseLength] =
        refuse $ indent $ translate $ do
          english "At least one length parameter is negative."
          german "Mindestens eines der 'length'-Parameter ist negativ."

    | minClauseLength > maxClauseLength =
        refuse $ indent $ translate $ do
          english "The minimum clause length is greater than the maximum clause length."
          german "Die untere Grenze der Klausellänge ist höher als die obere."


    | length usedLiterals < minClauseLength =
        refuse $ indent $ translate $ do
          german "Zu wenige Literale für diese Klausellänge."
          english "There are not enough literals available for this clause length."

    | null usedLiterals =
        refuse $ indent $ translate $ do
          german "Es wurden keine Literale angegeben."
          english "You did not specify which literals should be used."

    | otherwise = pure ()



checkCnfConf :: OutputMonad m => CnfConfig -> LangM m
checkCnfConf CnfConfig {..}
    | any (<1) [minClauseAmount, maxClauseAmount] =
        refuse $ indent $ translate $ do
          german "Mindestens eines der 'amount'-Parater ist negativ."
          english "At least one amount parameter is negative."

    | minClauseAmount > maxClauseAmount =
        refuse $ indent $ translate $ do
          german "Die untere Grenze der Klauselanzahl ist höher als die obere."
          english "The minimum amount of clauses is greater than the maximum amount."

    | minClauseAmount > 2 ^ min (maxClauseLength baseConf) (length (usedLiterals baseConf)) =
        refuse $ indent $ translate $ do
          german "Mit diesen Einstellungen kann die gewünschte Anzahl und Länge an Klauseln nicht erfüllt werden."
          english "There are not enough combinations available to satisfy your amount and length settings."

    | otherwise = checkBaseConf baseConf
