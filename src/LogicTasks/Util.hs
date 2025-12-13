{-# LANGUAGE RecordWildCards #-}
module LogicTasks.Util
       ( module Util
       , genCnf'
       , genDnf'
       , displayFormula
       , usesAllAtoms
       , isEmptyFormula
       , hasMinAmountOfAtoms
       ) where


import Util
import Test.QuickCheck (Gen)
import Formula.Types (Cnf, genCnf, genDnf, Dnf)
import Config (NormalFormConfig (..), BaseConfig(..), FormulaInst (..), FormulaConfig (..))
import Trees.Print (simplestDisplay)
import Tasks.SynTree.Config (SynTreeConfig(minAmountOfUniqueAtoms, availableAtoms))
import Formula.Util (isEmptyCnf, hasEmptyClause, isEmptyDnf, hasEmptyCon)

genCnf' :: NormalFormConfig -> Gen Cnf
genCnf' (NormalFormConfig{baseConf = BaseConfig{..}, ..})
  = genCnf (minClauseAmount,maxClauseAmount) (minClauseLength, maxClauseLength) usedAtoms True

genDnf' :: NormalFormConfig -> Gen Dnf
genDnf' (NormalFormConfig{baseConf = BaseConfig{..}, ..})
  = genDnf (minClauseAmount,maxClauseAmount) (minClauseLength, maxClauseLength) usedAtoms True

displayFormula :: FormulaInst -> String
displayFormula (InstCnf c) = show c
displayFormula (InstDnf d) = show d
displayFormula (InstArbitrary t) = simplestDisplay t

usesAllAtoms :: FormulaConfig -> Bool
usesAllAtoms (FormulaArbitrary syntaxTreeConfig)
  = minAmountOfUniqueAtoms syntaxTreeConfig == fromIntegral (length (availableAtoms syntaxTreeConfig))
usesAllAtoms _ = True -- Cnf and Dnf always uses all atoms

hasMinAmountOfAtoms :: Int -> FormulaConfig -> Bool
hasMinAmountOfAtoms minAmount (FormulaArbitrary syntaxTreeConfig) = minAmountOfUniqueAtoms syntaxTreeConfig >= fromIntegral minAmount
hasMinAmountOfAtoms minAmount formulaConfig = case formulaConfig of
  (FormulaCnf normalFormConf) -> check normalFormConf
  (FormulaDnf normalFormConf) -> check normalFormConf
  where
    check normalFormConf = length (usedAtoms (baseConf normalFormConf)) >= minAmount

isEmptyFormula :: FormulaInst -> Bool
isEmptyFormula (InstCnf cnf) = isEmptyCnf cnf || hasEmptyClause cnf
isEmptyFormula (InstDnf dnf) = isEmptyDnf dnf || hasEmptyCon dnf
isEmptyFormula (InstArbitrary _) = False
