module FormulaSpec where


import qualified Control.Exception as Exc (evaluate)

import Test.Hspec
import Test.QuickCheck
import LogicTasks.Formula



allocGen :: Gen Allocation
allocGen = do
    alloc <- arbitrary
    let flipped = flipNegs alloc
    pure flipped
  where
    flipNegs = map flipper
    flipper x
        | negative first = (opposite first,not (snd x))
        | otherwise      = x
      where
        first = fst x
    negative (Not _) = True
    negative _ = False



validBoundsClause :: Gen ((Int,Int),[Char])
validBoundsClause = do
    validChars <- sublistOf ['A'..'Z']
    let upperBound = length validChars
    lower <- chooseInt (1,upperBound)
    upper <- chooseInt (lower,upperBound)
    pure ((lower,upper),validChars)



validBoundsCnf :: Gen ((Int,Int),(Int,Int),[Char])
validBoundsCnf = do
    ((minLen,maxLen),chars) <- validBoundsClause
    let upperBound = min (2^maxLen) (2^length chars)
    minNum <- chooseInt (1,upperBound)
    maxNum <- chooseInt (minNum,upperBound)
    pure ((minNum,maxNum),(minLen,maxLen),chars)



spec :: Spec
spec = do
  describe "genLiteral" $ do
    it "should throw an error when called with the empty list" $
      Exc.evaluate (genLiteral []) `shouldThrow` errorCall "Can not construct Literal from empty list."
    it "should generate a random literal from the given char list" $
      property $ \chars -> not (null chars) ==> forAll (genLiteral chars) $ \char -> letter char `elem` chars


  describe "genClause" $ do
    it "should return the empty Clause when called with the empty list" $
      property $ \bounds -> forAll (genClause bounds []) isEmptyClause
    it "should return the empty Clause when called with invalid boundaries" $
      property $ \lower upper lits -> lower <= 0 || upper < lower || lower > length lits
                   ==> forAll (genClause (lower,upper) lits) isEmptyClause
    it "should generate a random clause of the correct length if given valid parameters" $
      forAll validBoundsClause $ \((lower,upper),chars) -> forAll (genClause (lower,upper) chars) $ \clause ->
        let len = length (literals clause) in len >= lower && len <= upper


  describe "genCnf" $ do
    it "should return the empty conjuncion when called with the empty list" $
      property $ \bounds1 bounds2 -> forAll (genCnf bounds1 bounds2 []) isEmptyCnf
    it "should generate a random cnf formula with a correct amount of clauses if given valid parameters" $
      forAll validBoundsCnf $ \((lowerNum,upperNum),(lowerLen,upperLen),chars) ->
        forAll (genCnf (lowerNum,upperNum) (lowerLen,upperLen) chars) $ \cnf ->
          let
            num = length (getClauses cnf)
          in
            num >= lowerNum && num <= upperNum
    it "should generate a random cnf formula with the correct clause length if given valid parameters" $
      forAll validBoundsCnf $ \((lowerNum,upperNum),(lowerLen,upperLen),chars) ->
        forAll (genCnf (lowerNum,upperNum) (lowerLen,upperLen) chars) $ \cnf ->
         let
           sizes = map (length . literals) (getClauses cnf)
         in
           maximum sizes <= upperLen && minimum sizes >= lowerLen

