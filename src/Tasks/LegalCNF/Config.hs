{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}


module Tasks.LegalCNF.Config(
    checkLegalCNFConfig,
    defaultLegalCNFConfig,
    LegalCNFInst(..),
    LegalCNFConfig(..),
) where


import Control.Monad.Output(OutputMonad(..), LangM, english, german, translate)
import Data.Char (isLetter)
import Data.Set (Set)
import GHC.Generics
import Formula.Types (lengthBound)

import Config(CnfConfig(..), BaseConfig(..), dCnfConf)




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
  , maxStringSize = 30
  , minStringSize = 4
  , allowArrowOperators = True
  }

checkLegalCNFConfig :: OutputMonad m => LegalCNFConfig -> LangM m
checkLegalCNFConfig LegalCNFConfig{cnfConfig = CnfConfig {baseConf = BaseConfig{..}, ..}, ..}
    | not (all isLetter usedLiterals)
      = reject "Only letters are allowed as literals."
               "Nur Buchstaben können Literale sein."
    | negArgs
      = reject
        "These parameters need to be greater than zero: minClauseAmount, minClauseLength, minStringSize, formulas."
        "Diese Parameter müssen größer als null sein: minClauseAmount, minClauseLength, minStringSize, formulas."
    | zeroArgs
      = reject "The following parameters need to be zero or greater: illegals, externalGenFormulas."
               "Diese Parameter müssen null oder größer sein: illegals, externalGenFormulas."
    | boundsError
      = reject "At least one upper bound is smaller than its corresponding lower bound."
               "Mindestens eine Obergrenze ist niedriger als die zugehörige Untergrenze."
    | (maxClauseLength > 2 * length usedLiterals) || (externalGenFormulas > 0 && maxClauseLength > length usedLiterals)
      = reject "The Used Literals can not generate a Clause with maxClauseLength"
               "Die angegebenen Literale können die maximale Klauselgröße nicht generieren."
    | let limit beginNumber = product (take maxClauseLength (reverse [1 .. beginNumber :: Integer])),
      let literalLength = fromIntegral (length usedLiterals),
      fromIntegral maxClauseAmount > min 15 (limit (2 * literalLength))
    || externalGenFormulas > 0 && fromIntegral maxClauseAmount > min 15 (limit literalLength)
      = reject "The maxClauseAmount is too big. There is the risk of duplicate clauses in the CNF."
               "maxClauseAmount ist zu groß. Es ist möglich, dass eine Klausel mehrfach in der CNF vorkommt."
    | fromIntegral formulas >
       (fromIntegral (maxClauseLength-minClauseLength+1)^(fromIntegral (maxClauseAmount-minClauseAmount+1) :: Integer))
       `div` (2 :: Integer) + 1
      = reject  "Amount of Formulas is too big and bears the risk of generating similar CNFs."
                "Menge an Formeln ist zu groß. Eine Formeln könnte mehrfach generiert werden."
    | maxClauseLength == 1 && maxClauseAmount == 1
      = reject "Atomic propositions have no illegal forms"
               "Atomare Aussagen können nicht syntaktisch falsch sein."
    | formulas - illegals - externalGenFormulas <
        (if includeFormWithJustOneClause then 1 else 0) + (if includeFormWithJustOneLiteralPerClause then 1 else 0)
      = reject "The formulas used to generate special formula are not sufficient."
               "Die Formeln zur Generierung der Spezialformel reichen nicht aus."
    | externalGenFormulas > 0
        && minClauseAmount > lengthBound minClauseLength (length usedLiterals) (minClauseLength, maxClauseLength)
      = reject "minClauseAmount is too large. The external generator can not generate a CNF."
               "minClauseAmount ist zu groß. Es kann keine passende Cnf geriert werden."
    | minStringSize < max 1 minClauseAmount * ((minClauseLength - 1) * 5 + 1)
      = reject "Can not generate String with given minStringSize."
               "String kann mit gegebenen minStringSize nicht generiert werden."
    | maxStringSize > maxClauseAmount * (maxClauseLength * 6 + 5)
      = reject "Can not generate String with given maxStringSize."
               "String kann mit gegebenen maxStringSize nicht generiert werden."
    | otherwise
      = pure()
  where
    reject e g  = refuse $ indent $ translate $ do
      english e
      german g

    negArgs = any (<1) [minClauseAmount, minClauseLength, minStringSize, formulas]
    zeroArgs = any (<0) [illegals, externalGenFormulas]
    boundsError = any (\(a,b) -> b < a)
      [(minClauseAmount,maxClauseAmount),(minClauseLength,maxClauseLength),(minStringSize,maxStringSize)]


data LegalCNFInst =
    LegalCNFInst
    {
        serialsOfWrong :: Set Int
      , formulaStrings :: [String]
    } deriving (Show,Generic)
