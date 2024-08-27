{-# LANGUAGE RecordWildCards #-}
module LogicTasks.Util (module Util, genCnf', genDnf') where


import Util
import Test.QuickCheck (Gen)
import Formula.Types (Cnf, genCnf, genDnf, Dnf)
import Config (CnfConfig (..), BaseConfig(..))

genCnf' :: CnfConfig -> Gen Cnf
genCnf' (CnfConfig{baseConf = BaseConfig{..}, ..}) = genCnf (minClauseAmount,maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals

genDnf' :: CnfConfig -> Gen Dnf
genDnf' (CnfConfig{baseConf = BaseConfig{..}, ..}) = genDnf (minClauseAmount,maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals
