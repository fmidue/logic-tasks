{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Tasks.LegalCNF.Config (
    LegalCNFConfig(..),
    LegalCNFInst(..),
    checkLegalCNFConfig,
    defaultLegalCNFConfig
    ) where


import Control.Monad.Output (LangM, OutputMonad, english, german, Language)
import Data.Char (isLetter)
import Data.Set (Set)
import Data.Map (Map)
import GHC.Generics (Generic)

import Config (BaseConfig(..), CnfConfig(..), dCnfConf)
import Formula.Types (lengthBound)
import LogicTasks.Helpers (reject)
import Util (checkCnfConf)




data LegalCNFConfig =
  LegalCNFConfig
  {
      cnfConfig :: CnfConfig
    , formulas :: Int
    , externalGenFormulas :: Int
    , illegals :: Int
    , includeFormWithJustOneClause :: Bool
    , includeFormWithJustOneLiteralPerClause :: Bool
    , maxStringSize :: Int
    , minStringSize :: Int
    , allowArrowOperators :: Bool
    , printSolution :: Bool
    , extraText :: Maybe (Map Language String)
  } deriving (Show,Generic)



defaultLegalCNFConfig :: LegalCNFConfig
defaultLegalCNFConfig =
  LegalCNFConfig
  {
    cnfConfig = dCnfConf
  , formulas = 4
  , externalGenFormulas = 1
  , illegals = 2
  , includeFormWithJustOneClause = False
  , includeFormWithJustOneLiteralPerClause = True
  , maxStringSize = 35
  , minStringSize = 12
  , allowArrowOperators = True
  , printSolution = True
  , extraText = Nothing
  }



checkLegalCNFConfig :: OutputMonad m => LegalCNFConfig -> LangM m
checkLegalCNFConfig LegalCNFConfig{cnfConfig = cnfConf@CnfConfig {baseConf = BaseConfig{..}, ..}, ..}
    | not (all isLetter usedLiterals) = reject $ do
        english "Only letters are allowed as literals."
        german "Nur Buchstaben können Literale sein."
    | negArgs = reject $ do
        english "These parameters need to be greater than zero: minClauseAmount, minClauseLength, minStringSize, formulas."
        german "Diese Parameter müssen größer als null sein: minClauseAmount, minClauseLength, minStringSize, formulas."
    | zeroArgs = reject $ do
        english "The following parameters need to be zero or greater: illegals, externalGenFormulas."
        german "Diese Parameter müssen null oder größer sein: illegals, externalGenFormulas."
    | boundsError = reject $ do
        english "At least one upper bound is smaller than its corresponding lower bound."
        german "Mindestens eine Obergrenze ist niedriger als die zugehörige Untergrenze."
    | (maxClauseLength > 2 * length usedLiterals) || (externalGenFormulas > 0 && maxClauseLength > length usedLiterals)
      = reject $ do
        english "The used literals cannot generate a clause with maxClauseLength."
        german "Die angegebenen Literale können die maximale Klauselgröße nicht generieren."
    | fromIntegral formulas >
       (fromIntegral (maxClauseLength-minClauseLength+1)^(fromIntegral (maxClauseAmount-minClauseAmount+1) :: Integer))
       `div` (2 :: Integer) + 1
      = reject $ do
        english "Amount of Formulas is too big and bears the risk of generating similar CNFs."
        german "Menge an Formeln ist zu groß. Eine Formeln könnte mehrfach generiert werden."
    | maxClauseLength == 1 && maxClauseAmount == 1 = reject $ do
        english "Atomic propositions have no illegal forms"
        german "Atomare Aussagen können nicht syntaktisch falsch sein."
    | formulas - illegals - externalGenFormulas <
        (if includeFormWithJustOneClause then 1 else 0) + (if includeFormWithJustOneLiteralPerClause then 1 else 0)
      = reject $ do
        english "The formulas used to generate special formula are not sufficient."
        german "Die Formeln zur Generierung der Spezialformel reichen nicht aus."
    | externalGenFormulas > 0
        && minClauseAmount > lengthBound (length usedLiterals) maxClauseLength
      = reject $ do
        english "minClauseAmount is too large. The external generator cannot generate a CNF."
        german "minClauseAmount ist zu groß. Es kann keine passende Cnf geriert werden."
    | minStringSize < max 1 minClauseAmount * ((minClauseLength - 1) * 5 + 1) = reject $ do
        english "Cannot generate string with given minStringSize."
        german "String kann mit gegebenen minStringSize nicht generiert werden."
    | maxStringSize > maxClauseAmount * (maxClauseLength * 6 + 5) = reject $ do
        english "Cannot generate string with given maxStringSize."
        german "String kann mit gegebenen maxStringSize nicht generiert werden."
    | otherwise = checkCnfConf cnfConf
  where
    negArgs = any (<1) [minClauseAmount, minClauseLength, minStringSize, formulas]
    zeroArgs = any (<0) [illegals, externalGenFormulas]
    boundsError = any (\(a,b) -> b < a)
      [(minClauseAmount,maxClauseAmount),(minClauseLength,maxClauseLength),(minStringSize,maxStringSize)]



data LegalCNFInst =
    LegalCNFInst
    {
        serialsOfWrong :: Set Int
      , formulaStrings :: [String]
      , showSolution :: Bool
      , addText :: Maybe (Map Language String)
    } deriving (Show,Generic)
