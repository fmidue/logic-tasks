{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}

module Config where


import Data.Typeable
import GHC.Generics
import Formula.Types
import Formula.Util
import Data.Map (Map)
import Control.Monad.Output (Language)



newtype Number = Number {value :: Maybe Int} deriving (Typeable, Generic)


newtype StepAnswer = StepAnswer {step :: Maybe (Literal, Clause)} deriving (Typeable, Generic)

instance Show StepAnswer where
  show (StepAnswer (Just (b,c))) = '(' : show b ++ ',' : ' ' : show c ++ ")"
  show _ = ""



data PickInst = PickInst {
                 cnfs    :: ![Cnf]
               , correct :: !Int
               , addText :: Maybe (Map Language String)
               }
               deriving (Typeable, Generic)

dPickInst :: PickInst
dPickInst =  PickInst
          { cnfs = [mkCnf [mkClause [Literal 'A', Not 'B']], mkCnf [mkClause [Not 'A', Literal 'B']]]
          , correct = 1
          , addText = Nothing
          }



data MaxInst = MaxInst {
                 cnf     :: !Cnf
               , addText :: Maybe (Map Language String)
               }
               deriving (Typeable, Generic)

dMaxInst :: MaxInst
dMaxInst =  MaxInst
          { cnf = mkCnf [mkClause [Literal 'A', Not 'B']]
          , addText = Nothing
          }




data MinInst = MinInst {
                 dnf     :: !Dnf
               , addText :: Maybe (Map Language String)
               }
               deriving (Typeable, Generic)

dMinInst :: MinInst
dMinInst =  MinInst
          { dnf = mkDnf [mkCon [Literal 'A', Not 'B']]
          , addText = Nothing
          }



data FillInst = FillInst {
                 cnf     :: !Cnf
               , missing :: ![Int]
               , addText :: Maybe (Map Language String)
               }
               deriving (Typeable, Generic)

dFillInst :: FillInst
dFillInst =  FillInst
          { cnf = mkCnf [mkClause [Literal 'A', Not 'B']]
          , missing = [1,4]
          , addText = Nothing
          }



data DecideInst = DecideInst {
                 cnf     :: !Cnf
               , changed :: ![Int]
               , addText :: Maybe (Map Language String)
               }
               deriving (Typeable, Generic)

dDecideInst :: DecideInst
dDecideInst =  DecideInst
          { cnf = mkCnf [mkClause [Literal 'A', Not 'B']]
          , changed = [1,4]
          , addText = Nothing
          }



data StepInst = StepInst {
                 clause1 :: !Clause
               , clause2 :: !Clause
               , addText :: Maybe (Map Language String)
               }
               deriving (Typeable, Generic)

dStepInst :: StepInst
dStepInst =  StepInst
          { clause1 = mkClause [Not 'A', Not 'C', Literal 'B']
          , clause2 = mkClause [Literal 'A', Not 'C']
          , addText = Nothing
          }



data ResolutionInst = ResolutionInst {
                 clauses :: ![Clause]
               , addText    :: Maybe (Map Language String)
               }
               deriving (Typeable, Generic)

dResInst :: ResolutionInst
dResInst =  ResolutionInst
          { clauses =
              [ mkClause [Not 'A', Not 'C', Literal 'B']
              , mkClause [Literal 'A', Not 'C']
              , mkClause [Literal 'C']
              , mkClause [Not 'B']
              ]
          , addText = Nothing
          }




data PrologInst = PrologInst {
                 literals1 :: !PrologClause
               , literals2 :: !PrologClause
               , addText :: Maybe (Map Language String)
               }
               deriving (Show, Typeable, Generic)


dPrologInst :: PrologInst
dPrologInst =  PrologInst
          { literals1 = mkPrologClause [PrologLiteral True "pred" ["fact"]]
          , literals2 = mkPrologClause [PrologLiteral False "pred" ["fact"]]
          , addText = Nothing
          }




data BaseConfig = BaseConfig
    { minClauseLength :: Int
    , maxClauseLength :: Int
    , usedLiterals :: String
    } deriving (Typeable, Generic, Show)


dBaseConf :: BaseConfig
dBaseConf = BaseConfig {
      minClauseLength = 2
    , maxClauseLength = 3
    , usedLiterals = "ABCD"
    }



data CnfConfig = CnfConfig
    { baseConf:: BaseConfig
    , minClauseAmount :: Int
    , maxClauseAmount :: Int
    } deriving (Typeable, Generic, Show)

dCnfConf :: CnfConfig
dCnfConf = CnfConfig
    { baseConf = dBaseConf
    , minClauseAmount = 2
    , maxClauseAmount = 4
    }




data PickConfig = PickConfig {
       cnfConf :: CnfConfig
     , amountOfOptions :: Int
     , pickCnf :: Bool
     , extraText :: Maybe (Map Language String)
     }
     deriving (Typeable, Generic)

dPickConf :: PickConfig
dPickConf = PickConfig
    { cnfConf = dCnfConf
    , amountOfOptions = 3
    , pickCnf = False
    , extraText = Nothing
    }



data FillConfig = FillConfig {
      cnfConf :: CnfConfig
    , percentageOfGaps :: Int
    , percentTrueEntries :: Maybe (Int,Int)
    , extraText :: Maybe (Map Language String)
    }
    deriving (Typeable, Generic)

dFillConf :: FillConfig
dFillConf = FillConfig
    { cnfConf = dCnfConf
    , percentageOfGaps = 40
    , percentTrueEntries = Just (30,70)
    , extraText = Nothing
    }



data MinMaxConfig = MinMaxConfig {
      cnfConf :: CnfConfig
    , percentTrueEntries :: Maybe (Int,Int)
    , extraText :: Maybe (Map Language String)
    }
    deriving (Typeable, Generic)

dMinMaxConf :: MinMaxConfig
dMinMaxConf = MinMaxConfig
    { cnfConf = dCnfConf
    , percentTrueEntries = Just (50,70)
    , extraText = Nothing
    }



data DecideConfig = DecideConfig {
      cnfConf :: CnfConfig
    , percentageOfChanged :: Int
    , extraText :: Maybe (Map Language String)
    }
    deriving (Typeable, Generic)

dDecideConf :: DecideConfig
dDecideConf = DecideConfig
    { cnfConf = dCnfConf
    , percentageOfChanged = 40
    , extraText = Nothing
    }



data StepConfig = StepConfig {
      baseConf :: BaseConfig
    , extraText :: Maybe (Map Language String)
    }
    deriving (Typeable, Generic)

dStepConf :: StepConfig
dStepConf = StepConfig
    { baseConf = dBaseConf
    , extraText = Nothing
    }



data PrologConfig = PrologConfig {
      minClauseLength :: Int
    , maxClauseLength :: Int
    , usedPredicates :: [PrologLiteral]
    , extraText :: Maybe (Map Language String)
    , firstClauseShape :: ClauseShape
    , secondClauseShape :: ClauseShape
    }
    deriving (Show, Typeable, Generic)

dPrologConf :: PrologConfig
dPrologConf = PrologConfig
    { minClauseLength = 1
    , maxClauseLength = 3
    , usedPredicates = [PrologLiteral True "f" ["a"], PrologLiteral True "f" ["b"], PrologLiteral True "g" ["a"]]
    , extraText = Nothing
    , firstClauseShape = HornClause Query
    , secondClauseShape = HornClause Procedure
    }


data ResolutionConfig = ResolutionConfig {
      baseConf :: BaseConfig
    , minSteps :: Int
    , extraText :: Maybe (Map Language String)
    }
    deriving (Typeable, Generic)

dResConf :: ResolutionConfig
dResConf = ResolutionConfig
    { baseConf = dBaseConf
    , minSteps = 2
    , extraText = Nothing
    }
