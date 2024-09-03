module FormulaSpec where


import qualified Control.Exception as Exc (evaluate)

import Test.Hspec
import Test.QuickCheck
import LogicTasks.Formula
import LogicTasks.Config
import LogicTasks.Util
import Formula.Types (lengthBound)
import Control.OutputCapable.Blocks (Language(German))
import Control.OutputCapable.Blocks.Debug (checkConfigWith)

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
    let lowerBound = (length chars `div` minLen) + 1
    let upperBound = min 50 (lengthBound (length chars) maxLen)
    minNum <- chooseInt (lowerBound, upperBound)
    if minNum > lengthBound (length chars) minLen
      then validBoundsCnf
      else do
        maxNum <- chooseInt (minNum,upperBound)
        pure ((minNum,maxNum),(minLen,maxLen),chars)

spec :: Spec
spec = do
  describe "genValidBoundsClause" $
    it "should generate valid bounds" $
      forAll validBoundsClause $ \((l,u),cs) ->
        ioProperty $ checkConfigWith German (BaseConfig l u cs) checkBaseConf

  describe "genValidBoundsCnf" $
    it "should generate valid bounds" $
      withMaxSuccess 1000 $ forAll validBoundsCnf $ \((l1,u1),(l2,u2),cs) ->
        ioProperty $ checkConfigWith German (CnfConfig (BaseConfig l2 u2 cs) l1 u1) checkCnfConf

  describe "genLiteral" $ do
    it "should throw an error when called with the empty list" $
      Exc.evaluate (genLiteral []) `shouldThrow` errorCall "Cannot construct literal from empty list."
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
        let numOfLiterals = length (literals clause) in numOfLiterals >= lower && numOfLiterals <= upper


  describe "genCnf" $ do
    it "should return the empty conjunction when called with the empty list" $
      property $ \bounds1 bounds2 -> forAll (genCnf bounds1 bounds2 []) isEmptyCnf
    it "should generate a random cnf formula with a correct amount of clauses if given valid parameters" $
      forAll validBoundsCnf $ \((lowerNum,upperNum),(lowerLen,upperLen),chars) ->
        forAll (genCnf (lowerNum,upperNum) (lowerLen,upperLen) chars) $ \cnf' ->
          let
            num = length (getClauses cnf')
          in
            num >= lowerNum && num <= upperNum
    it "should generate a random cnf formula with the correct clause length if given valid parameters" $
      forAll validBoundsCnf $ \((lowerNum,upperNum),(lowerLen,upperLen),chars) ->
        forAll (genCnf (lowerNum,upperNum) (lowerLen,upperLen) chars) $ \cnf' ->
         let
           sizes = map (length . literals) (getClauses cnf')
         in
           maximum sizes <= upperLen && minimum sizes >= lowerLen
    it "should generate a random cnf formula containing all given atoms" $
      forAll validBoundsCnf $ \((lowerNum,upperNum),(lowerLen,upperLen),chars) ->
        forAll (genCnf (lowerNum,upperNum) (lowerLen,upperLen) chars) $ \cnf' ->
          all (\c -> Literal c `elem` atomics cnf') chars

  describe "genDnf" $ do
    it "should return the empty disjunction when called with the empty list" $
      property $ \bounds1 bounds2 -> forAll (genDnf bounds1 bounds2 []) isEmptyDnf
    it "should generate a random dnf formula with a correct amount of cons if given valid parameters" $
      forAll validBoundsCnf $ \((lowerNum,upperNum),(lowerLen,upperLen),chars) ->
        forAll (genDnf (lowerNum,upperNum) (lowerLen,upperLen) chars) $ \dnf' ->
          let
            num = length (getConjunctions dnf')
          in
            num >= lowerNum && num <= upperNum
    it "should generate a random dnf formula with the correct con length if given valid parameters" $
      forAll validBoundsCnf $ \((lowerNum,upperNum),(lowerLen,upperLen),chars) ->
        forAll (genDnf (lowerNum,upperNum) (lowerLen,upperLen) chars) $ \dnf' ->
         let
           sizes = map (length . literals) (getConjunctions dnf')
         in
           maximum sizes <= upperLen && minimum sizes >= lowerLen
    it "should generate a random dnf formula containing all given atoms" $
      forAll validBoundsCnf $ \((lowerNum,upperNum),(lowerLen,upperLen),chars) ->
        forAll (genDnf (lowerNum,upperNum) (lowerLen,upperLen) chars) $ \dnf' ->
          all (\c -> Literal c `elem` atomics dnf') chars
