{-# LANGUAGE CPP #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
#if !MIN_VERSION_base(4,18,0)
{-# LANGUAGE DerivingStrategies #-}
#endif
{-# language DuplicateRecordFields #-}

module Config where


import Data.Data (Data)
#if !MIN_VERSION_base(4,18,0)
import Data.Typeable (Typeable)
#endif
import GHC.Generics
import Formula.Types
import Formula.Util
import Data.Map (Map)
import Control.OutputCapable.Blocks (Language (..))
import Tasks.SynTree.Config (SynTreeConfig (..))
import qualified Trees.Types as ST (BinOp(..), SynTree(..))
import Trees.Formula ()


data FormulaConfig
  = FormulaCnf NormalFormConfig
  | FormulaDnf NormalFormConfig
  | FormulaArbitrary SynTreeConfig
  deriving (Show,Generic)

data FormulaInst
  = InstCnf Cnf
  | InstDnf Dnf
  | InstArbitrary (ST.SynTree ST.BinOp Char)
  deriving (Show,Eq,Generic,Data)

instance Formula FormulaInst where
  literals (InstCnf c) = literals c
  literals (InstDnf d) = literals d
  literals (InstArbitrary t) = literals t

  atomics (InstCnf c) = atomics c
  atomics (InstDnf d) = atomics d
  atomics (InstArbitrary t) = atomics t

  amount (InstCnf c) = amount c
  amount (InstDnf d) = amount d
  amount (InstArbitrary t) = amount t

  evaluate x (InstCnf c) = evaluate x c
  evaluate x (InstDnf d) = evaluate x d
  evaluate x (InstArbitrary t) = evaluate x t

instance ToSAT FormulaInst where
  convert (InstCnf c) = convert c
  convert (InstDnf d) = convert d
  convert (InstArbitrary t) = convert t


newtype Number = Number {value :: Maybe Int}
  deriving (Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif


newtype StepAnswer = StepAnswer {step :: Maybe (Literal, Clause)}
  deriving Generic
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

instance Show StepAnswer where
  show (StepAnswer (Just (b,c))) = '(' : show b ++ ',' : ' ' : show c ++ ")"
  show _ = ""

data DecideChoice
  = Correct
  | Wrong
  | NoAnswer
  deriving (Show,Ord,Eq,Enum,Bounded,Generic)

showChoice :: Language -> DecideChoice -> String
showChoice German Correct = "Richtig"
showChoice German Wrong = "Fehlerhaft"
showChoice German NoAnswer = "Keine Antwort"
showChoice English Correct = "Correct"
showChoice English Wrong = "Wrong"
showChoice English NoAnswer = "No answer"

data PickInst = PickInst {
                 formulas :: [FormulaInst]
               , correct :: !Int
               , showSolution :: Bool
               , addText :: Maybe (Map Language String)
               }
  deriving (Data, Eq, Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

dPickInst :: PickInst
dPickInst =  PickInst
          { formulas = [InstCnf $ mkCnf [mkClause [Positive 'A', Negative 'B']], InstCnf $ mkCnf [mkClause [Negative 'A', Positive 'B']]]
          , correct = 1
          , showSolution = False
          , addText = Nothing
          }



data MaxInst = MaxInst {
                 cnf     :: !Cnf
               , showSolution :: Bool
               , addText :: Maybe (Map Language String)
               , unicodeAllowed :: Bool
               }
  deriving (Data, Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

dMaxInst :: MaxInst
dMaxInst =  MaxInst
          { cnf = mkCnf [mkClause [Positive 'A', Negative 'B']]
          , showSolution = False
          , addText = Nothing
          , unicodeAllowed = False
          }




data MinInst = MinInst {
                 dnf :: !Dnf
               , showSolution :: Bool
               , addText :: Maybe (Map Language String)
               , unicodeAllowed :: Bool
               }
  deriving (Data, Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

dMinInst :: MinInst
dMinInst =  MinInst
          { dnf = mkDnf [mkCon [Positive 'A', Negative 'B']]
          , showSolution = False
          , addText = Nothing
          , unicodeAllowed = False
          }



data FillInst = FillInst {
                 formula :: FormulaInst
               , missing :: ![Int]
               , missingValues :: [Bool]
               , showSolution :: Bool
               , addText :: Maybe (Map Language String)
               }
  deriving (Data, Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

dFillInst :: FillInst
dFillInst =  FillInst
          { formula = InstCnf $ mkCnf [mkClause [Positive 'A', Negative 'B']]
          , missing = [1,4]
          , missingValues = [True, True]
          , showSolution = False
          , addText = Nothing
          }



data DecideInst = DecideInst {
                 formula :: FormulaInst
               , changed :: ![Int]
               , showSolution :: Bool
               , addText :: Maybe (Map Language String)
               }
  deriving (Data, Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

dDecideInst :: DecideInst
dDecideInst =  DecideInst
          { formula = InstCnf $ mkCnf [mkClause [Positive 'A', Negative 'B']]
          , changed = [1,4]
          , showSolution = False
          , addText = Nothing
          }



data StepInst = StepInst {
                 clause1 :: !Clause
               , clause2 :: !Clause
               , solution :: (Literal, Clause)
               , usesSetNotation :: Bool
               , showSolution :: Bool
               , addText :: Maybe (Map Language String)
               , unicodeAllowed :: Bool
               }
  deriving (Data, Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

dStepInst :: StepInst
dStepInst =  StepInst
          { clause1 = mkClause [Negative 'A', Negative 'C', Positive 'B']
          , clause2 = mkClause [Positive 'A', Negative 'C']
          , solution = (Positive 'A', mkClause [Negative 'C', Positive 'B'])
          , usesSetNotation = False
          , showSolution = False
          , addText = Nothing
          , unicodeAllowed = False
          }



data ResolutionInst = ResolutionInst {
                 clauses :: ![Clause]
               , solution :: [ResStep]
               , printFeedbackImmediately :: Bool
               , usesSetNotation :: Bool
               , showSolution :: Bool
               , addText    :: Maybe (Map Language String)
               , unicodeAllowed :: Bool
               }
  deriving (Data, Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

dResInst :: ResolutionInst
dResInst = let
            nAnCpB = mkClause [Negative 'A', Negative 'C', Positive 'B']
            pAnC = mkClause [Positive 'A', Negative 'C']
            pC = mkClause [Positive 'C']
            nB = mkClause [Negative 'B']
            pA = mkClause [Positive 'A']
            nC = mkClause [Negative 'C']
            nCpB = mkClause [Negative 'C', Positive 'B']
              in ResolutionInst
                { clauses =
                    [ nAnCpB
                    , pAnC
                    , pC
                    , nB
                    ]
                , solution =
                    [ Res (Left pAnC  , Left pC, (pA, Nothing))
                    , Res (Left nAnCpB, Left pA, (nCpB, Nothing))
                    , Res (Left nCpB  , Left nB, (nC, Nothing))
                    , Res (Left nC    , Left pC, (mkClause [], Nothing))
                    ]
                , printFeedbackImmediately = True
                , usesSetNotation = True
                , showSolution = False
                , addText = Nothing
                , unicodeAllowed = False
                }




data PrologInst = PrologInst {
                 literals1 :: !PrologClause
               , literals2 :: !PrologClause
               , solution :: (PrologLiteral, PrologClause)
               , usesSetNotation :: Bool
               , showSolution :: Bool
               , addText :: Maybe (Map Language String)
               }
  deriving (Data, Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif


dPrologInst :: PrologInst
dPrologInst =  PrologInst
          { literals1 = mkPrologClause [PrologLiteral True "pred" ["fact"]]
          , literals2 = mkPrologClause [PrologLiteral False "pred" ["fact"]]
          , solution = (PrologLiteral True "pred" ["fact"], mkPrologClause [])
          , usesSetNotation = False
          , showSolution = False
          , addText = Nothing
          }




data BaseConfig = BaseConfig
    { minClauseLength :: Int
    , maxClauseLength :: Int
    , usedAtoms :: String
    }
  deriving (Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif


dBaseConf :: BaseConfig
dBaseConf = BaseConfig {
      minClauseLength = 2
    , maxClauseLength = 3
    , usedAtoms = "ABCD"
    }



data NormalFormConfig = NormalFormConfig
    { baseConf:: BaseConfig
    , minClauseAmount :: Int
    , maxClauseAmount :: Int
    }
  deriving (Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

dNormalFormConf :: NormalFormConfig
dNormalFormConf = NormalFormConfig
    { baseConf = dBaseConf
    , minClauseAmount = 2
    , maxClauseAmount = 4
    }




data PickConfig = PickConfig {
       formulaConfig :: FormulaConfig
     , amountOfOptions :: Int
     , percentTrueEntries :: Maybe (Int,Int)
     , printSolution :: Bool
     , extraText :: Maybe (Map Language String)
     }
  deriving (Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

dPickConf :: PickConfig
dPickConf = PickConfig
    { formulaConfig = FormulaCnf dNormalFormConf
    , amountOfOptions = 3
    , percentTrueEntries = Just (30,70)
    , printSolution = False
    , extraText = Nothing
    }



data FillConfig = FillConfig {
      formulaConfig :: FormulaConfig
    , percentageOfGaps :: Int
    , percentTrueEntries :: Maybe (Int,Int)
    , printSolution :: Bool
    , extraText :: Maybe (Map Language String)
    }
  deriving (Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

dFillConf :: FillConfig
dFillConf = FillConfig
    { formulaConfig = FormulaCnf dNormalFormConf
    , percentageOfGaps = 40
    , percentTrueEntries = Just (30,70)
    , printSolution = False
    , extraText = Nothing
    }



data MinMaxConfig = MinMaxConfig {
      normalFormConf :: NormalFormConfig
    , percentTrueEntries :: Maybe (Int,Int)
    , printSolution :: Bool
    , extraText :: Maybe (Map Language String)
    , offerUnicodeInput :: Bool
    }
  deriving (Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

dMinMaxConf :: MinMaxConfig
dMinMaxConf = MinMaxConfig
    { normalFormConf = dNormalFormConf
    , percentTrueEntries = Just (50,70)
    , printSolution = False
    , extraText = Nothing
    , offerUnicodeInput = False
    }



data DecideConfig = DecideConfig {
      formulaConfig :: FormulaConfig
    , percentageOfChanged :: Int
    , percentTrueEntries :: Maybe (Int,Int)
    , printSolution :: Bool
    , extraText :: Maybe (Map Language String)
    }
  deriving (Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

dDecideConf :: DecideConfig
dDecideConf = DecideConfig
    { formulaConfig = FormulaCnf dNormalFormConf
    , percentageOfChanged = 40
    , percentTrueEntries = Just (30,70)
    , printSolution = False
    , extraText = Nothing
    }



data StepConfig = StepConfig {
      baseConf :: BaseConfig
    , useSetNotation :: Bool
    , printSolution :: Bool
    , extraText :: Maybe (Map Language String)
    , offerUnicodeInput :: Bool
    }
  deriving (Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

dStepConf :: StepConfig
dStepConf = StepConfig
    { baseConf = dBaseConf
    , useSetNotation = False
    , printSolution = False
    , extraText = Nothing
    , offerUnicodeInput = False
    }



data PrologConfig = PrologConfig {
      minClauseLength :: Int
    , maxClauseLength :: Int
    , usedPredicates :: [PrologLiteral]
    , extraText :: Maybe (Map Language String)
    , printSolution :: Bool
    , firstClauseShape :: ClauseShape
    , secondClauseShape :: ClauseShape
    , useSetNotation :: Bool
    }
  deriving (Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

dPrologConf :: PrologConfig
dPrologConf = PrologConfig
    { minClauseLength = 1
    , maxClauseLength = 3
    , usedPredicates = [PrologLiteral True "f" ["a"], PrologLiteral True "f" ["b"], PrologLiteral True "g" ["a"]]
    , extraText = Nothing
    , printSolution = False
    , firstClauseShape = HornClause Query
    , secondClauseShape = HornClause Procedure
    , useSetNotation = False
    }


data ResolutionConfig = ResolutionConfig {
      baseConf :: BaseConfig
    , minSteps :: Int
    , printFeedbackImmediately :: Bool
    , useSetNotation :: Bool
    , printSolution :: Bool
    , extraText :: Maybe (Map Language String)
    , offerUnicodeInput :: Bool
    }
  deriving (Generic, Show)
#if !MIN_VERSION_base(4,18,0)
  deriving Typeable
#endif

dResConf :: ResolutionConfig
dResConf = ResolutionConfig
    { baseConf = dBaseConf
    , minSteps = 2
    , printFeedbackImmediately = True
    , useSetNotation = True
    , printSolution = False
    , extraText = Nothing
    , offerUnicodeInput = False
    }
