{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}

module Config where


import Data.Typeable
import GHC.Generics
import Formula.Types
import Formula.Util
import Data.Map (Map)
import Control.Monad.Output (Language)
import Tasks.SynTree.Config (SynTreeConfig (..), defaultSynTreeConfig)
import qualified Trees.Types as ST (BinOp(..), SynTree(..))



newtype Number = Number {value :: Maybe Int} deriving (Show,Typeable, Generic)


newtype StepAnswer = StepAnswer {step :: Maybe (Literal, Clause)} deriving (Typeable, Generic)

instance Show StepAnswer where
  show (StepAnswer (Just (b,c))) = '(' : show b ++ ',' : ' ' : show c ++ ")"
  show _ = ""



data PickInst = PickInst {
                 trees :: [ST.SynTree ST.BinOp Char]
               , correct :: !Int
               , showSolution :: Bool
               , addText :: Maybe (Map Language String)
               }
               deriving (Typeable, Generic, Show, Eq)

dPickInst :: PickInst
dPickInst =  PickInst
          { trees = [ST.Binary ST.And
            (ST.Binary ST.Or (ST.Leaf 'A') (ST.Not (ST.Leaf 'B')))
            (ST.Binary ST.Or (ST.Not (ST.Leaf 'A')) (ST.Leaf 'B'))]
          , correct = 1
          , showSolution = False
          , addText = Nothing
          }



data MaxInst = MaxInst {
                 cnf     :: !Cnf
               , showSolution :: Bool
               , addText :: Maybe (Map Language String)
               }
               deriving (Typeable, Generic)

dMaxInst :: MaxInst
dMaxInst =  MaxInst
          { cnf = mkCnf [mkClause [Literal 'A', Not 'B']]
          , showSolution = False
          , addText = Nothing
          }




data MinInst = MinInst {
                 dnf :: !Dnf
               , showSolution :: Bool
               , addText :: Maybe (Map Language String)
               }
               deriving (Typeable, Generic)

dMinInst :: MinInst
dMinInst =  MinInst
          { dnf = mkDnf [mkCon [Literal 'A', Not 'B']]
          , showSolution = False
          , addText = Nothing
          }



data FillInst = FillInst {
                 tree :: ST.SynTree ST.BinOp Char
               , missing :: ![Int]
               , showSolution :: Bool
               , addText :: Maybe (Map Language String)
               }
               deriving (Typeable, Generic, Show)

dFillInst :: FillInst
dFillInst =  FillInst
          { tree = ST.Binary ST.And (ST.Leaf 'A') (ST.Not (ST.Leaf 'B'))
          , missing = [1,4]
          , showSolution = False
          , addText = Nothing
          }



data DecideInst = DecideInst {
                 tree :: ST.SynTree ST.BinOp Char
               , changed :: ![Int]
               , showSolution :: Bool
               , addText :: Maybe (Map Language String)
               }
               deriving (Typeable, Generic, Show)

dDecideInst :: DecideInst
dDecideInst =  DecideInst
          { tree = ST.Binary ST.And (ST.Leaf 'A') (ST.Not (ST.Leaf 'B'))
          , changed = [1,4]
          , showSolution = False
          , addText = Nothing
          }



data StepInst = StepInst {
                 clause1 :: !Clause
               , clause2 :: !Clause
               , showSolution :: Bool
               , addText :: Maybe (Map Language String)
               }
               deriving (Typeable, Generic)

dStepInst :: StepInst
dStepInst =  StepInst
          { clause1 = mkClause [Not 'A', Not 'C', Literal 'B']
          , clause2 = mkClause [Literal 'A', Not 'C']
          , showSolution = False
          , addText = Nothing
          }



data ResolutionInst = ResolutionInst {
                 clauses :: ![Clause]
               , printFeedbackImmediately :: Bool
               , showSolution :: Bool
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
          , printFeedbackImmediately = True
          , showSolution = False
          , addText = Nothing
          }




data PrologInst = PrologInst {
                 literals1 :: !PrologClause
               , literals2 :: !PrologClause
               , showSolution :: Bool
               , addText :: Maybe (Map Language String)
               }
               deriving (Show, Typeable, Generic)


dPrologInst :: PrologInst
dPrologInst =  PrologInst
          { literals1 = mkPrologClause [PrologLiteral True "pred" ["fact"]]
          , literals2 = mkPrologClause [PrologLiteral False "pred" ["fact"]]
          , showSolution = False
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
       syntaxTreeConfig :: SynTreeConfig
     , amountOfOptions :: Int
     , printSolution :: Bool
     , extraText :: Maybe (Map Language String)
     }
     deriving (Typeable, Generic, Show)

dPickConf :: PickConfig
dPickConf = PickConfig
    { syntaxTreeConfig = defaultSynTreeConfig {
        availableAtoms = "ABC"
      , minAmountOfUniqueAtoms = 3
      }
    , amountOfOptions = 3
    , printSolution = False
    , extraText = Nothing
    }



data FillConfig = FillConfig {
      syntaxTreeConfig :: SynTreeConfig
    , percentageOfGaps :: Int
    , percentTrueEntries :: Maybe (Int,Int)
    , printSolution :: Bool
    , extraText :: Maybe (Map Language String)
    }
    deriving (Typeable, Generic, Show)

dFillConf :: FillConfig
dFillConf = FillConfig
    { syntaxTreeConfig = defaultSynTreeConfig
    , percentageOfGaps = 40
    , percentTrueEntries = Just (30,70)
    , printSolution = False
    , extraText = Nothing
    }



data MinMaxConfig = MinMaxConfig {
      cnfConf :: CnfConfig
    , percentTrueEntries :: Maybe (Int,Int)
    , printSolution :: Bool
    , extraText :: Maybe (Map Language String)
    }
    deriving (Typeable, Generic)

dMinMaxConf :: MinMaxConfig
dMinMaxConf = MinMaxConfig
    { cnfConf = dCnfConf
    , percentTrueEntries = Just (50,70)
    , printSolution = False
    , extraText = Nothing
    }



data DecideConfig = DecideConfig {
      syntaxTreeConfig :: SynTreeConfig
    , percentageOfChanged :: Int
    , printSolution :: Bool
    , extraText :: Maybe (Map Language String)
    }
    deriving (Typeable, Generic, Show)

dDecideConf :: DecideConfig
dDecideConf = DecideConfig
    { syntaxTreeConfig = defaultSynTreeConfig
    , percentageOfChanged = 40
    , printSolution = False
    , extraText = Nothing
    }



data StepConfig = StepConfig {
      baseConf :: BaseConfig
    , printSolution :: Bool
    , extraText :: Maybe (Map Language String)
    }
    deriving (Typeable, Generic)

dStepConf :: StepConfig
dStepConf = StepConfig
    { baseConf = dBaseConf
    , printSolution = False
    , extraText = Nothing
    }



data PrologConfig = PrologConfig {
      minClauseLength :: Int
    , maxClauseLength :: Int
    , usedPredicates :: [PrologLiteral]
    , extraText :: Maybe (Map Language String)
    , printSolution :: Bool
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
    , printSolution = False
    , firstClauseShape = HornClause Query
    , secondClauseShape = HornClause Procedure
    }


data ResolutionConfig = ResolutionConfig {
      baseConf :: BaseConfig
    , minSteps :: Int
    , printFeedbackImmediately :: Bool
    , printSolution :: Bool
    , extraText :: Maybe (Map Language String)
    }
    deriving (Typeable, Generic)

dResConf :: ResolutionConfig
dResConf = ResolutionConfig
    { baseConf = dBaseConf
    , minSteps = 2
    , printFeedbackImmediately = True
    , printSolution = False
    , extraText = Nothing
    }
