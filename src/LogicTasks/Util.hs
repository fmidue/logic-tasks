{-# LANGUAGE RecordWildCards #-}
module LogicTasks.Util
       ( module Util
       , genCnf'
       , genDnf'
       , display'
       , usesAllAtoms
       ) where


import Util
import Test.QuickCheck (Gen)
import Formula.Types (Cnf, genCnf, genDnf, Dnf)
import Config (CnfConfig (..), BaseConfig(..), FormulaInst (..), FormulaConfig (..))
import Trees.Print (display)
import Tasks.SynTree.Config (SynTreeConfig(minAmountOfUniqueAtoms, availableAtoms))

genCnf' :: CnfConfig -> Gen Cnf
genCnf' (CnfConfig{baseConf = BaseConfig{..}, ..}) = genCnf (minClauseAmount,maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals

genDnf' :: CnfConfig -> Gen Dnf
genDnf' (CnfConfig{baseConf = BaseConfig{..}, ..}) = genDnf (minClauseAmount,maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals

display' :: FormulaInst -> String
display' (InstCnf c) = show c
display' (InstDnf d) = show d
display' (InstArbitrary t) = display t

usesAllAtoms :: FormulaConfig -> Bool
usesAllAtoms (FormulaArbitrary syntaxTreeConfig)
  = minAmountOfUniqueAtoms syntaxTreeConfig == fromIntegral (length (availableAtoms syntaxTreeConfig))
usesAllAtoms _ = True
