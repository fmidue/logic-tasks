
module Tasks.LegalNormalForm.GenerateIllegal (
    genIllegalCnfSynTree,
    genIllegalDnfSynTree
    ) where


import Formula.Types
    ( genCnf, genDnf, genClause, genCon )
import qualified Formula.Types as SetFormula hiding (Dnf(..), Con(..))
import qualified Formula.Types as SetFormulaDnf (Dnf(..),Con(..))

import Control.Monad (join)
import Data.List ((\\))
import Data.Set (size, toList, Set)
import Test.QuickCheck (Gen, choose, elements, frequency)
import Test.QuickCheck.Gen (oneof)

import Trees.Helpers (clauseToSynTree, collectLeaves, literalToSynTree, relabelShape, conToSynTree)
import Trees.Types (BinOp(..), SynTree(..), allBinaryOperators)

genIllegalCnfSynTree :: (Int,Int) -> (Int,Int) -> [Char] -> Bool -> Gen (SynTree BinOp Char)
genIllegalCnfSynTree =
  genIllegalSynTree genCnf genClause SetFormula.clauseSet SetFormula.literalSet clauseToSynTree And Or

genIllegalDnfSynTree :: (Int,Int) -> (Int,Int) -> [Char] -> Bool -> Gen (SynTree BinOp Char)
genIllegalDnfSynTree =
  genIllegalSynTree genDnf genCon SetFormulaDnf.clauseSet SetFormulaDnf.literalSet conToSynTree Or And

genIllegalSynTree ::
-- jscpd:ignore-start
    ((Int,Int) -> (Int,Int) -> [Char] -> Bool -> Gen a)
    -- ^^ generator for CNF/DNF
    -> ((Int, Int) -> [Char] -> Gen b)
    -- ^^ generator for Clause/Con
    -> (a -> Set b)
    -- ^^ getter for clauseSet/conSet from CNF/DNF
    -> (b -> Set SetFormula.Literal)
    -- ^^ getter for literalSet from Clause/Con
    -> (b -> SynTree BinOp Char)
    -- ^^ mapper Clause/Con -> SynTree
    -> BinOp
    -- ^^ characterizing operator for CNF/DNF
    -> BinOp
    -- ^^ characterizing operator for Clause/Con
    -> (Int,Int)
    -> (Int,Int)
    -> [Char]
    -> Bool
    -> Gen (SynTree BinOp Char)
-- jscpd:ignore-end
genIllegalSynTree
  genF
  genS
  getC
  getL
  cToS
  charOpF
  charOpC
  (minClauseAmount, maxClauseAmount)
  (minClauseLength, maxClauseLength)
  usedLiterals
  allowArrowOperators = do
    ifUseError <- elements [True,False]
    let ifUseError'
          | maxClauseAmount == 1 = False
          | maxClauseLength == 1 = True
          | otherwise = ifUseError
     in if ifUseError'
        then do
            clauses <- choose (max 2 minClauseAmount, maxClauseAmount)
            firstSyntaxShape <- genIllegalFormulaShape charOpF charOpC allowArrowOperators (clauses - 1)
            clauseList <- toList . getC
              <$> genF (clauses, clauses) (minClauseLength, maxClauseLength) usedLiterals False
            return (genIllegal firstSyntaxShape cToS clauseList)
        else do
            clauses <- choose (minClauseAmount, maxClauseAmount)
            genWithOneIllegalClause
              genF
              genS
              getC
              getL
              cToS
              charOpF
              charOpC
              (minClauseLength, maxClauseLength)
              usedLiterals
              (clauses - 1)
              allowArrowOperators



genWithOneIllegalClause ::
    ((Int,Int) -> (Int,Int) -> [Char] -> Bool -> Gen a)
    -- ^^ generator for CNF/DNF
    -> ((Int, Int) -> [Char] -> Gen b)
    -- ^^ generator for Clause/Con
    -> (a -> Set b)
    -- ^^ getter for clauseSet/conSet from CNF/DNF
    -> (b -> Set SetFormula.Literal)
    -- ^^ getter for literalSet from Clause/Con
    -> (b -> SynTree BinOp Char)
    -- ^^ mapper Clause/Con -> SynTree
    -> BinOp
    -- ^^ characterizing operator for CNF/DNF
    -> BinOp
    -- ^^characterizing operator for Clause/Con
    -> (Int,Int)
    -> [Char]
    -> Int
    -> Bool
    -> Gen (SynTree BinOp Char)
genWithOneIllegalClause
  genF
  genS
  getC
  getL
  cToS
  charOpF
  charOpC
  (minClauseLength, maxClauseLength)
  usedLiterals
  ands
  allowArrowOperators = do
        clauseList <- toList . getC <$>
          genF (ands, ands) (minClauseLength, maxClauseLength) usedLiterals False
        illegalTree' <- illegalTree genS getL charOpC charOpF (minClauseLength, maxClauseLength) usedLiterals allowArrowOperators
        let illLength = length (collectLeaves illegalTree')
            (first, second) = span (\x -> illLength >= size (getL x)) clauseList
            headTrees = map cToS first
            tailTrees = map cToS second
        return (foldr1 (Binary And) (headTrees ++ (illegalTree' : tailTrees)))



genIllegal ::
    SynTree BinOp ()
    -> (a -> SynTree BinOp Char)
    -- ^^ mapper Clause/Con -> SynTree
    -> [a]
    -> SynTree BinOp Char
genIllegal treeShape toTree = join . relabelShape treeShape . map toTree



illegalTree ::
    ((Int,Int) -> [Char] -> Gen b)
    -- ^^ generator for Clause/Con
    -> (b -> Set SetFormula.Literal)
    -- ^^ getter for literalSet from Clause/Con
    -> BinOp
    -- ^^ characterizing operator for Clause/Con
    -> BinOp
    -- ^^ flipped characterizing operator for Clause/Con
    -> (Int,Int)
    -> [Char]
    -> Bool
    -> Gen (SynTree BinOp Char)
illegalTree gen getLiterals charOpC fCharOpC (minClauseLength, maxClauseLength) usedLiterals allowArrowOperators = do
    treeLength <- choose (max 2 minClauseLength, maxClauseLength)
    illegalSynTreeShape <- genIllegalShape charOpC fCharOpC True allowArrowOperators (treeLength - 1)
    leaves <- toList . getLiterals <$> gen (treeLength,treeLength) usedLiterals
    return (relabelShape illegalSynTreeShape leaves >>= literalToSynTree)


genIllegalShape ::
    BinOp
    -- ^^ characterizing operator in Clause/Con
    -> BinOp
    -- ^^ flipped characterizing operator in Clause/Con
    -> Bool
    -> Bool
    -> Int
    -> Gen (SynTree BinOp ())
genIllegalShape _ _ _ _ 0 = error "impossible"
genIllegalShape charOpC fCharOpC ifFirstLayer allowArrowOperators ors = do
    ifUseError <- frequency [(1, return True), (ors - 1, return False)]
    if ifUseError
    then  if allowArrowOperators
          then oneof [ return (Not (legalShape charOpC ors))
                     , genIllegalOperator (legalShape charOpC) (Equi : Impl : BackImpl : [fCharOpC | not ifFirstLayer]) ors
                     ]
          else  if ifFirstLayer
                then return (Not (legalShape charOpC ors))
                else oneof [ return (Not (legalShape charOpC ors))
                           , genIllegalOperator (legalShape charOpC) [fCharOpC] ors
                           ]
    else genIllegalShapeInSubTree ors (genIllegalShape charOpC fCharOpC False allowArrowOperators) charOpC



genIllegalFormulaShape ::
    BinOp
    -- ^^ characterizing operator for CNF/DNF
    -> BinOp
    -- ^^ characterizing operator for Clause/Con
    -> Bool
    -> Int
    -> Gen (SynTree BinOp ())
genIllegalFormulaShape _ _ _ 0 = error "impossible"
genIllegalFormulaShape charOpF charOpC True 1 = oneof [ return (Not (legalShape charOpF 1))
                                  , genIllegalOperator (legalShape charOpF) (allBinaryOperators \\ [charOpF, charOpC]) 1
                                  ]
genIllegalFormulaShape charOpF _ False 1 = return (Not (legalShape charOpF 1))
genIllegalFormulaShape charOpF charOpC allowArrowOperators ands = do
    ifUseError <- frequency [(1, return True), (ands - 1, return False)]
    if ifUseError
    then oneof [ return (Not (legalShape charOpF ands))
               , genIllegalOperator (legalShape charOpF)
                   (if allowArrowOperators then allBinaryOperators \\ [charOpF] else [charOpC]) ands
               ]
    else genIllegalShapeInSubTree ands (genIllegalFormulaShape charOpF charOpC allowArrowOperators) charOpF

------

genIllegalShapeInSubTree :: Int -> (Int -> Gen (SynTree BinOp ())) -> BinOp -> Gen (SynTree BinOp ())
genIllegalShapeInSubTree amount illegalFunc operator = do
    operatorsIllegalSide <- choose (1, amount - 1)
    node <- elements [Binary operator, flip (Binary operator)]
    illegalSubTree <- illegalFunc operatorsIllegalSide
    return (node illegalSubTree (legalShape Or (amount - 1 - operatorsIllegalSide)))

genIllegalOperator :: (Int -> SynTree BinOp ()) -> [BinOp] -> Int -> Gen (SynTree BinOp ())
genIllegalOperator recF operators restOperators =
    do
        errorOperator <- elements operators
        leftOperators <- choose (0, restOperators - 1)
        return (Binary errorOperator (recF leftOperators) (recF (restOperators - 1 - leftOperators)))



legalShape :: BinOp -> Int -> SynTree BinOp ()
legalShape operator amount = foldr (Binary operator . Leaf) (Leaf ()) (replicate amount ())
