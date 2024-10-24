{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}

module Config where


import Data.Typeable
import GHC.Generics
import Formula.Types
import Formula.Util
import Data.Map (Map)
import Control.OutputCapable.Blocks (Language)
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
  deriving (Show,Eq,Generic)

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


newtype Number = Number {value :: Maybe Int} deriving (Show,Typeable, Generic)


newtype StepAnswer = StepAnswer {step :: Maybe (Literal, Clause)} deriving (Typeable, Generic)

instance Show StepAnswer where
  show (StepAnswer (Just (b,c))) = '(' : show b ++ ',' : ' ' : show c ++ ")"
  show _ = ""



data PickInst = PickInst {
                 formulas :: [FormulaInst]
               , correct :: !Int
               , showSolution :: Bool
               , addText :: Maybe (Map Language String)
               }
               deriving (Typeable, Generic, Show, Eq)

dPickInst :: PickInst
dPickInst =  PickInst
          { formulas = [InstCnf $ mkCnf [mkClause [Literal 'A', Not 'B']], InstCnf $ mkCnf [mkClause [Not 'A', Literal 'B']]]
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
               deriving (Typeable, Generic)

dMaxInst :: MaxInst
dMaxInst =  MaxInst
          { cnf = mkCnf [mkClause [Literal 'A', Not 'B']]
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
               deriving (Typeable, Generic)

dMinInst :: MinInst
dMinInst =  MinInst
          { dnf = mkDnf [mkCon [Literal 'A', Not 'B']]
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
               deriving (Typeable, Generic, Show)

dFillInst :: FillInst
dFillInst =  FillInst
          { formula = InstCnf $ mkCnf [mkClause [Literal 'A', Not 'B']]
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
               deriving (Typeable, Generic, Show)

dDecideInst :: DecideInst
dDecideInst =  DecideInst
          { formula = InstCnf $ mkCnf [mkClause [Literal 'A', Not 'B']]
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
               deriving (Typeable, Generic)

dStepInst :: StepInst
dStepInst =  StepInst
          { clause1 = mkClause [Not 'A', Not 'C', Literal 'B']
          , clause2 = mkClause [Literal 'A', Not 'C']
          , solution = (Literal 'A', mkClause [Not 'C', Literal 'B'])
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
               deriving (Typeable, Generic, Show)

dResInst :: ResolutionInst
dResInst = let
            nAnCpB = mkClause [Not 'A', Not 'C', Literal 'B']
            pAnC = mkClause [Literal 'A', Not 'C']
            pC = mkClause [Literal 'C']
            nB = mkClause [Not 'B']
            pA = mkClause [Literal 'A']
            nC = mkClause [Not 'C']
            nCpB = mkClause [Not 'C', Literal 'B']
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
               deriving (Show, Typeable, Generic)


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
    , usedLiterals :: String
    } deriving (Typeable, Generic, Show)


dBaseConf :: BaseConfig
dBaseConf = BaseConfig {
      minClauseLength = 2
    , maxClauseLength = 3
    , usedLiterals = "ABCD"
    }



data NormalFormConfig = NormalFormConfig
    { baseConf:: BaseConfig
    , minClauseAmount :: Int
    , maxClauseAmount :: Int
    } deriving (Typeable, Generic, Show)

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
     deriving (Typeable, Generic, Show)

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
    deriving (Typeable, Generic, Show)

dFillConf :: FillConfig
dFillConf = FillConfig
    { formulaConfig = FormulaCnf dNormalFormConf
    , percentageOfGaps = 40
    , percentTrueEntries = Just (30,70)
    , printSolution = False
    , extraText = Nothing
    }



data MinMaxConfig = MinMaxConfig {
      cnfConf :: NormalFormConfig
    , percentTrueEntries :: Maybe (Int,Int)
    , printSolution :: Bool
    , extraText :: Maybe (Map Language String)
    , offerUnicodeInput :: Bool
    }
    deriving (Typeable, Generic)

dMinMaxConf :: MinMaxConfig
dMinMaxConf = MinMaxConfig
    { cnfConf = dNormalFormConf
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
    deriving (Typeable, Generic, Show)

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
    deriving (Typeable, Generic)

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
    deriving (Typeable, Generic, Show)

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
