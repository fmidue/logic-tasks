{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module PrologSpec where
import Test.Hspec
import LogicTasks.Semantics.Prolog (genPrologInst, verifyQuiz, description, verifyStatic, partialGrade', completeGrade')
import Config (dPrologConf, PrologInst (..), PrologConfig (..))
import Formula.Helpers (hasTheClauseShape)
import Formula.Types (
  ClauseShape(..),
  PrologLiteral(..),
  anyClause,
  anyHornClause,
  factClause,
  procedureClause,
  queryClause
  )
import Test.QuickCheck
import Control.OutputCapable.Blocks (LangM)
import TestHelpers (doesNotRefuse)


genClauseShapes :: Gen (ClauseShape,ClauseShape)
genClauseShapes = do
  first <- elements [
    anyClause,
    factClause,
    procedureClause,
    queryClause,
    anyHornClause
    ]
  second <- elements $ [
    anyClause,
    anyHornClause,
    procedureClause
    ] ++
    if first == factClause || first == queryClause
      then []
      else [factClause,queryClause]
  pure (first, second)



genPrologLiteral :: Gen PrologLiteral
genPrologLiteral = PrologLiteral <$> arbitrary <*> listOf1 (elements ['a'..'z']) <*> listOf1 (listOf1 $ elements ['a'..'z'])


validBoundsPrologConfig :: Gen PrologConfig
validBoundsPrologConfig = do
  -- going any higher than this causes tests to run seemingly indefinitely
  min <- chooseInt (1,8)
  max <- chooseInt (min,12)
  printSolution <- arbitrary
  useSetNotation <- arbitrary
  (firstClauseShape,secondClauseShape) <- genClauseShapes
  let minClauseLength = if factClause `elem` [firstClauseShape,secondClauseShape] then 1 else min
  let maxClauseLength = if procedureClause `elem` [firstClauseShape,secondClauseShape] && max < 2 then 2 else max
  usedPredicates <- vectorOf maxClauseLength genPrologLiteral
  pure $ PrologConfig {
    minClauseLength,
    maxClauseLength,
    usedPredicates,
    extraText = Nothing,
    printSolution,
    firstClauseShape,
    secondClauseShape,
    useSetNotation
    }


spec :: Spec
spec = do
  describe "config" $ do
    it "default config should pass config check" $
      doesNotRefuse (verifyQuiz dPrologConf :: LangM Maybe)
    it "validBoundsPrologConfig should generate a valid config" $
      forAll validBoundsPrologConfig $ \config ->
        doesNotRefuse (verifyQuiz config :: LangM Maybe)
  describe "description" $ do
    it "should not reject" $
      forAll validBoundsPrologConfig $ \config ->
        forAll (genPrologInst config) $ \inst ->
          doesNotRefuse (description inst :: LangM Maybe)
  describe "genPrologInst" $ do
    it "should pass verifyStatic" $
      forAll validBoundsPrologConfig $ \config ->
        forAll (genPrologInst config) $ \inst ->
          doesNotRefuse (verifyStatic inst :: LangM Maybe)
    it "should pass grading with correct answer" $
      forAll validBoundsPrologConfig $ \config ->
        forAll (genPrologInst config) $ \inst ->
          doesNotRefuse (partialGrade' inst (solution inst) :: LangM Maybe) &&
          doesNotRefuse (completeGrade' inst (solution inst) :: LangM Maybe)
    it "should only generate PrologInst with horn clauses by default" $
      forAll validBoundsPrologConfig $ \config ->
        forAll (genPrologInst config) $ \PrologInst {..} ->
          hasTheClauseShape (firstClauseShape config) literals1
            && hasTheClauseShape (secondClauseShape config) literals2
