{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module PrologSpec where
import Test.Hspec
import LogicTasks.Semantics.Prolog (genPrologInst, verifyQuiz, description, verifyStatic, partialGrade', completeGrade')
import Config (dPrologConf, PrologInst (..), PrologConfig (..))
import Formula.Helpers (hasTheClauseShape)
import Test.QuickCheck
import Control.OutputCapable.Blocks (LangM)
import TestHelpers (doesNotRefuse)
import Formula.Types (PrologLiteral (..), ClauseShape (..), HornShape (..))
import Data.List (singleton)
import Data.Containers.ListUtils (nubOrd)


validBoundsClauseShape :: Gen ClauseShape
validBoundsClauseShape = oneof
  [ pure AnyClause
  , pure $ HornClause AnyHornClause
  , pure $ HornClause Fact
  , pure $ HornClause Procedure
  , pure $ HornClause Query
  ]

validBoundsPrologLiteral :: Gen PrologLiteral
validBoundsPrologLiteral = do
  polarity <- elements [True, False]
  name <- singleton <$> elements "fghijklmn" -- no-spell-check
  constantAmount <- choose (1,5)
  constants <- vectorOf constantAmount (singleton <$> elements "abcdeuvwxyz") -- no-spell-check

  return $ PrologLiteral
    { polarity
    , name
    , constants
    }


validBoundsPrologConfig :: Gen PrologConfig
validBoundsPrologConfig = do
  minClauseLength <- choose (1, 5)
  maxClauseLength <- choose (minClauseLength, 5)
  let clauseShape = validBoundsClauseShape `suchThat` shapeSuitsClauseBounds (minClauseLength, maxClauseLength)
  (firstClauseShape, secondClauseShape) <- ((,) <$> clauseShape <*> clauseShape)
    `suchThat` \(f,s) -> not ((f `elem` [HornClause Fact, HornClause Query]) && f == s)
  let minPredicateAmount =
        case (firstClauseShape, secondClauseShape) of
          (HornClause Procedure, HornClause Procedure) ->
            max 2 minClauseLength + 1

          (HornClause Procedure, _) ->
            max 2 minClauseLength

          (_, HornClause Procedure) ->
            max 2 minClauseLength

          _ ->
            minClauseLength

  predicateAmount <- choose (minPredicateAmount, max minPredicateAmount maxClauseLength)
  usedPredicates <- vectorOf predicateAmount validBoundsPrologLiteral
    `suchThat` \predicates ->
      length (nubOrd (map (\p -> p { polarity = True }) predicates))
        == length predicates

  return $ PrologConfig
    { minClauseLength
    , maxClauseLength
    , usedPredicates
    , extraText = []
    , printSolution = True
    , firstClauseShape
    , secondClauseShape
    , useSetNotation = False
    }
  where
    shapeSuitsClauseBounds (minB, _) (HornClause Fact) = minB == 1
    shapeSuitsClauseBounds (_, maxB) (HornClause Procedure) = maxB >= 2
    shapeSuitsClauseBounds _ _ = True

spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (verifyQuiz dPrologConf :: LangM Maybe)
    it "validBoundsPrologConfig should pass config check" $
      forAll validBoundsPrologConfig $ \config ->
          doesNotRefuse (verifyQuiz config :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll validBoundsPrologConfig $ \config ->
        within (30 * 1000000) $ forAll (genPrologInst config) $ \inst ->
          doesNotRefuse (description inst :: LangM Maybe)
  describe "genPrologInst" $ do
    it "should pass verifyStatic" $
      forAll validBoundsPrologConfig $ \config ->
        within (30 * 1000000) $ forAll (genPrologInst config) $ \inst ->
          doesNotRefuse (verifyStatic inst :: LangM Maybe)
    it "should pass grading with correct answer" $
      forAll validBoundsPrologConfig $ \config ->
        within (30 * 1000000) $ forAll (genPrologInst config) $ \inst ->
          doesNotRefuse (partialGrade' inst (solution inst) :: LangM Maybe) &&
          doesNotRefuse (completeGrade' inst (solution inst) :: LangM Maybe)
    it "should only generate PrologInst with horn clauses by default" $
      within (30 * 1000000) $ forAll (genPrologInst dPrologConf) $ \PrologInst {..} ->
        hasTheClauseShape (firstClauseShape dPrologConf) literals1
          && hasTheClauseShape (secondClauseShape dPrologConf) literals2
