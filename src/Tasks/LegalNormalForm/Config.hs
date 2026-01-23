{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Tasks.LegalNormalForm.Config (
    LegalNormalFormConfig(..),
    LegalNormalFormInst(..),
    TreeInfo(..),
    ErrorReason(..),
    treeIsErroneous,
    checkLegalNormalFormConfig,
    defaultLegalNormalFormConfig,
    ) where


import Control.OutputCapable.Blocks (LangM, OutputCapable, english, german, ExtraText (NoExtraText))
import Data.Char (isLetter)
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
    , printDetailedSolution :: Maybe Bool
    , extraText :: ExtraText
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
  , printDetailedSolution = Nothing
  , extraText = NoExtraText
  }


checkLegalNormalFormConfig :: OutputCapable m => LegalNormalFormConfig -> LangM m
checkLegalNormalFormConfig LegalNormalFormConfig{normalFormConfig = normalFormConf@NormalFormConfig {baseConf = BaseConfig{..}, ..}, ..}
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
    | fromIntegral formulas > maxFormulasBound = reject $ do
        english "'formulas' is too high. This bears the risk of generating a formula multiple times. "
        english $ "Reduce 'formulas' to at least " ++ show maxFormulasBound ++ ". "
        english "Alternatively, the distance of minClauseLength and maxClauseLength and/or minClauseAmount and maxClauseAmount could be increased."
        german "formulas ist zu groß. Eine Formel könnte mehrfach generiert werden. "
        german $ "formulas muss auf mindestens " ++ show maxFormulasBound ++ " verringert werden. "
        german "Stattdessen kann die Differenz zwischen minClauseLength und maxClauseLength und/oder minClauseAmount und maxClauseAmount erhöht werden."
    | maxClauseLength == 1 && maxClauseAmount == 1 = reject $ do
        english "Atomic formulas have no illegal forms."
        german "Atomare Formeln können nicht syntaktisch falsch sein."
    | formulas - illegals < amountNeededForSpecial = reject $ do
        english "The amount of valid formulas is too small to allow for your choice of includeFormWithJustOneClause and includeFormWithJustOneLiteralPerClause. "
        english $ "formulas - invalid should at least be " ++ show amountNeededForSpecial ++ "."
        german "Die Anzahl korrekter Formeln ist zu niedrig für die getroffene Auswahl von includeFormWithJustOneClause und includeFormWithJustOneLiteralPerClause. "
        german $ "formulas - invalid sollte mindestens " ++ show amountNeededForSpecial ++ " sein."
    | minClauseAmount > lengthBound (length usedAtoms) maxClauseLength
      = reject $ do
        english "minClauseAmount is too large. The generator cannot generate a normal form."
        german "minClauseAmount ist zu groß. Es kann keine passende Normalform geriert werden."
    | minStringSize < minStringSizeBound = reject $ do
        english "Cannot generate string with given minStringSize. "
        english $ "It needs to be raised to at least " ++ show minStringSizeBound ++ ". "
        english "Alternatively, minClauseAmount and/or minClauseLength could be reduced."
        german "String kann mit gegebenem minStringSize nicht generiert werden. "
        german $ "Es muss auf mindestens " ++ show minStringSizeBound ++ " erhöht werden. "
        german "Stattdessen kann minClauseAmount und/oder minClauseLength verringert werden."
    | maxStringSize > maxStringSizeBound = reject $ do
        english "Cannot generate string with given maxStringSize. "
        english $ "It needs to be reduced to at least " ++ show maxStringSizeBound ++ ". "
        english "Alternatively, maxClauseAmount and/or maxClauseLength could be increased."
        german "String kann mit gegebenem maxStringSize nicht generiert werden. "
        german $ "Es muss auf mindestens " ++ show maxStringSizeBound ++ " verringert werden. "
        german "Stattdessen kann maxClauseAmount und/oder maxClauseLength erhöht werden."
    | otherwise = checkNormalFormConfig normalFormConf
  where
    negArgs = any (<1) [minClauseAmount, minClauseLength, minStringSize, formulas]
    boundsError = any (\(a,b) -> b < a)
      [(minClauseAmount,maxClauseAmount),(minClauseLength,maxClauseLength),(minStringSize,maxStringSize)]
    amountNeededForSpecial = (if includeFormWithJustOneClause then 1 else 0) + (if includeFormWithJustOneLiteralPerClause then 1 else 0)
    maxFormulasBound =
      (fromIntegral (maxClauseLength-minClauseLength+1)^(fromIntegral (maxClauseAmount-minClauseAmount+1) :: Integer)) `div`
      (2 :: Integer) + 1
    minStringSizeBound = max 1 minClauseAmount * ((minClauseLength - 1) * 4 + 1)
    maxStringSizeBound = maxClauseAmount * (maxClauseLength * 6 + 5)

data TreeInfo = Correct | CorrectSingleClause | CorrectAtomicClauses | Erroneous ErrorReason
  deriving (Show, Generic)

data ErrorReason = IllegalNegation | IllegalOperator | OnClauseLevel ErrorReason
  deriving (Show, Generic)

treeIsErroneous :: TreeInfo -> Bool
treeIsErroneous (Erroneous _) = True
treeIsErroneous _ = False

data LegalNormalFormInst =
    LegalNormalFormInst
    {
      formulaInfos :: [(Int, TreeInfo, String)]
      , showSolution :: Maybe Bool
      , addText :: ExtraText
    } deriving (Show,Generic)
