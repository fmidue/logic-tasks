{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Tasks.LegalNormalForm.Config (
    LegalNormalFormConfig(..),
    LegalNormalFormInst(..),
    checkLegalNormalFormConfig,
    defaultLegalNormalFormConfig,
    ) where


import Control.OutputCapable.Blocks (LangM, Language, OutputCapable, english, german)
import Data.Char (isLetter)
import Data.Set (Set)
import Data.Map (Map)
import GHC.Generics (Generic)

import Config (BaseConfig(..), NormalFormConfig(..), dNormalFormConf)
import Formula.Types (lengthBound)
import LogicTasks.Helpers (reject)
import Util (checkNormalFormConfig)




data LegalNormalFormConfig =
  LegalNormalFormConfig
  {
      normalFormConfig :: NormalFormConfig
    , formulas :: Int
    , illegals :: Int
    , includeFormWithJustOneClause :: Bool
    , includeFormWithJustOneLiteralPerClause :: Bool
    , maxStringSize :: Int
    , minStringSize :: Int
    , allowArrowOperators :: Bool
    , printSolution :: Bool
    , extraText :: Maybe (Map Language String)
  } deriving (Show,Generic)



defaultLegalNormalFormConfig :: LegalNormalFormConfig
defaultLegalNormalFormConfig =
  LegalNormalFormConfig
  {
    normalFormConfig = dNormalFormConf
  , formulas = 4
  , illegals = 2
  , includeFormWithJustOneClause = False
  , includeFormWithJustOneLiteralPerClause = True
  , maxStringSize = 35
  , minStringSize = 12
  , allowArrowOperators = True
  , printSolution = False
  , extraText = Nothing
  }


checkLegalNormalFormConfig :: OutputCapable m => LegalNormalFormConfig -> LangM m
checkLegalNormalFormConfig LegalNormalFormConfig{normalFormConfig = cnfConf@NormalFormConfig {baseConf = BaseConfig{..}, ..}, ..}
    | not (all isLetter usedAtoms) = reject $ do
        english "Only letters are allowed as atomic formulas."
        german "Nur Buchstaben können atomare Formeln sein."
    | negArgs = reject $ do
        english "These parameters need to be greater than zero: minClauseAmount, minClauseLength, minStringSize, formulas."
        german "Diese Parameter müssen größer als null sein: minClauseAmount, minClauseLength, minStringSize, formulas."
    | illegals < 0  = reject $ do
        english "The following parameter needs to be zero or greater: illegals."
        german "Dieser Parameter muss null oder größer sein: illegals."
    | boundsError = reject $ do
        english "At least one upper bound is smaller than its corresponding lower bound."
        german "Mindestens eine Obergrenze ist niedriger als die zugehörige Untergrenze."
    | maxClauseLength > length usedAtoms
      = reject $ do
        english "The used atomic formulas cannot generate a clause with maxClauseLength."
        german "Die angegebenen atomaren Formeln können die maximale Klauselgröße nicht generieren."
    | fromIntegral formulas >
       (fromIntegral (maxClauseLength-minClauseLength+1)^(fromIntegral (maxClauseAmount-minClauseAmount+1) :: Integer))
       `div` (2 :: Integer) + 1
      = reject $ do
        english "Amount of formulas is too big and bears the risk of generating similar normal forms."
        german "Anzahl an Formeln ist zu groß. Eine Formel könnte mehrfach generiert werden."
    | maxClauseLength == 1 && maxClauseAmount == 1 = reject $ do
        english "Atomic formulas have no illegal forms."
        german "Atomare Formeln können nicht syntaktisch falsch sein."
    | formulas - illegals <
        (if includeFormWithJustOneClause then 1 else 0) + (if includeFormWithJustOneLiteralPerClause then 1 else 0)
      = reject $ do
        english "The formulas used to generate special formula are not sufficient."
        german "Die Formeln zur Generierung der Spezialformel reichen nicht aus."
    | minClauseAmount > lengthBound (length usedAtoms) maxClauseLength
      = reject $ do
        english "minClauseAmount is too large. The generator cannot generate a normal form."
        german "minClauseAmount ist zu groß. Es kann keine passende Normalform geriert werden."
    | minStringSize < max 1 minClauseAmount * ((minClauseLength - 1) * 4 + 1) = reject $ do
        english "Cannot generate string with given minStringSize."
        german "String kann mit gegebenem minStringSize nicht generiert werden."
    | maxStringSize > maxClauseAmount * (maxClauseLength * 6 + 5) = reject $ do
        english "Cannot generate string with given maxStringSize."
        german "String kann mit gegebenem maxStringSize nicht generiert werden."
    | otherwise = checkNormalFormConfig cnfConf
  where
    negArgs = any (<1) [minClauseAmount, minClauseLength, minStringSize, formulas]
    boundsError = any (\(a,b) -> b < a)
      [(minClauseAmount,maxClauseAmount),(minClauseLength,maxClauseLength),(minStringSize,maxStringSize)]

data LegalNormalFormInst =
    LegalNormalFormInst
    {
        serialsOfWrong :: Set Int
      , formulaStrings :: [String]
      , showSolution :: Bool
      , addText :: Maybe (Map Language String)
    } deriving (Show,Generic)
