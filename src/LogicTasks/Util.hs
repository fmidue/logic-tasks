{-# LANGUAGE RecordWildCards #-}
module LogicTasks.Util
       ( module Util
       , genCnf'
       , genDnf'
       , displayFormula
       , usesAllAtoms
       , isEmptyFormula
       ) where


import Util
import Test.QuickCheck (Gen)
import Formula.Types (Cnf, genCnf, genDnf, Dnf)
import Config (NormalFormConfig (..), BaseConfig(..), FormulaInst (..), FormulaConfig (..))
import Trees.Print (display)
import Tasks.SynTree.Config (SynTreeConfig(minAmountOfUniqueAtoms, availableAtoms))
import Formula.Util (isEmptyCnf, hasEmptyClause, isEmptyDnf, hasEmptyCon)

genCnf' :: NormalFormConfig -> Gen Cnf
genCnf' (NormalFormConfig{baseConf = BaseConfig{..}, ..})
  = genCnf (minClauseAmount,maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals True

genDnf' :: NormalFormConfig -> Gen Dnf
genDnf' (NormalFormConfig{baseConf = BaseConfig{..}, ..})
  = genDnf (minClauseAmount,maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals True

displayFormula :: FormulaInst -> String
displayFormula (InstCnf c) = show c
displayFormula (InstDnf d) = show d
displayFormula (InstArbitrary t) = display t

usesAllAtoms :: FormulaConfig -> Bool
usesAllAtoms (FormulaArbitrary syntaxTreeConfig)
  = minAmountOfUniqueAtoms syntaxTreeConfig == fromIntegral (length (availableAtoms syntaxTreeConfig))
usesAllAtoms _ = True -- Cnf and Dnf always uses all atoms

isEmptyFormula :: FormulaInst -> Bool
isEmptyFormula (InstCnf cnf) = isEmptyCnf cnf || hasEmptyClause cnf
isEmptyFormula (InstDnf dnf) = isEmptyDnf dnf || hasEmptyCon dnf
isEmptyFormula (InstArbitrary _) = False
