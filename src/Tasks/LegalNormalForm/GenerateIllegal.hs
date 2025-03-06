
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
import Tasks.LegalNormalForm.Config (ErrorReason(..))

genIllegalCnfSynTree :: (Int,Int) -> (Int,Int) -> [Char] -> Bool -> Gen (SynTree BinOp Char, ErrorReason)
genIllegalCnfSynTree =
  genIllegalSynTree genCnf genClause SetFormula.clauseSet SetFormula.literalSet clauseToSynTree And Or

genIllegalDnfSynTree :: (Int,Int) -> (Int,Int) -> [Char] -> Bool -> Gen (SynTree BinOp Char, ErrorReason)
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
    -> Gen (SynTree BinOp Char, ErrorReason)
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
  usedAtoms
  allowArrowOperators = do
    ifUseError <- elements [True,False]
    let ifUseError'
          | maxClauseAmount == 1 = False
          | maxClauseLength == 1 = True
          | otherwise = ifUseError
     in if ifUseError'
        then do
            clauses <- choose (max 2 minClauseAmount, maxClauseAmount)
            (firstSyntaxShape, errorReason) <- genIllegalFormulaShape charOpF charOpC allowArrowOperators (clauses - 1)
            clauseList <- toList . getC
              <$> genF (clauses, clauses) (minClauseLength, maxClauseLength) usedAtoms False
            return (genIllegal firstSyntaxShape cToS clauseList, errorReason)
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
              usedAtoms
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
    -> Gen (SynTree BinOp Char, ErrorReason)
genWithOneIllegalClause
  genF
  genS
  getC
  getL
  cToS
  charOpF
  charOpC
  (minClauseLength, maxClauseLength)
  usedAtoms
  ands
  allowArrowOperators = do
        clauseList <- toList . getC <$>
          genF (ands, ands) (minClauseLength, maxClauseLength) usedAtoms False
        (illegalTree', errorReason) <-
          illegalTree genS getL charOpC charOpF (minClauseLength, maxClauseLength) usedAtoms allowArrowOperators
        let illLength = length (collectLeaves illegalTree')
            (first, second) = span (\x -> illLength >= size (getL x)) clauseList
            headTrees = map cToS first
            tailTrees = map cToS second
        return (foldr1 (Binary And) (headTrees ++ (illegalTree' : tailTrees)), OnClauseLevel errorReason)



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
    -> Gen (SynTree BinOp Char, ErrorReason)
illegalTree gen getLiterals charOpC fCharOpC (minClauseLength, maxClauseLength) usedAtoms allowArrowOperators = do
    treeLength <- choose (max 2 minClauseLength, maxClauseLength)
    (illegalSynTreeShape, errorReason) <- genIllegalShape charOpC fCharOpC True allowArrowOperators (treeLength - 1)
    leaves <- toList . getLiterals <$> gen (treeLength,treeLength) usedAtoms
    return (relabelShape illegalSynTreeShape leaves >>= literalToSynTree, errorReason)


genIllegalShape ::
    BinOp
    -- ^^ characterizing operator in Clause/Con
    -> BinOp
    -- ^^ flipped characterizing operator in Clause/Con
    -> Bool
    -> Bool
    -> Int
    -> Gen (SynTree BinOp (), ErrorReason)
genIllegalShape _ _ _ _ 0 = error "impossible"
genIllegalShape charOpC fCharOpC ifFirstLayer allowArrowOperators ors = do
    ifUseError <- frequency [(1, return True), (ors - 1, return False)]
    if ifUseError
    then  if allowArrowOperators
          then oneof [ return (Not (legalShape charOpC ors), IllegalNegation)
                     , genIllegalOperator (legalShape charOpC) (Equi : Impl : BackImpl : [fCharOpC | not ifFirstLayer]) ors
                     ]
          else  if ifFirstLayer
                then return (Not (legalShape charOpC ors), IllegalNegation)
                else oneof [ return (Not (legalShape charOpC ors), IllegalNegation)
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
    -> Gen (SynTree BinOp (), ErrorReason)
genIllegalFormulaShape _ _ _ 0 = error "impossible"
genIllegalFormulaShape charOpF charOpC True 1 = oneof [ return (Not (legalShape charOpF 1), IllegalNegation)
                                  , genIllegalOperator (legalShape charOpF) (allBinaryOperators \\ [charOpF, charOpC]) 1
                                  ]
genIllegalFormulaShape charOpF _ False 1 = return (Not (legalShape charOpF 1), IllegalNegation)
genIllegalFormulaShape charOpF charOpC allowArrowOperators ands = do
    ifUseError <- frequency [(1, return True), (ands - 1, return False)]
    if ifUseError
    then oneof [ return (Not (legalShape charOpF ands), IllegalNegation)
               , genIllegalOperator (legalShape charOpF)
                   (if allowArrowOperators then allBinaryOperators \\ [charOpF] else [charOpC]) ands
               ]
    else genIllegalShapeInSubTree ands (genIllegalFormulaShape charOpF charOpC allowArrowOperators) charOpF

------

genIllegalShapeInSubTree :: Int -> (Int -> Gen (SynTree BinOp (), ErrorReason)) -> BinOp -> Gen (SynTree BinOp (), ErrorReason)
genIllegalShapeInSubTree amount illegalFunc operator = do
    operatorsIllegalSide <- choose (1, amount - 1)
    node <- elements [Binary operator, flip (Binary operator)]
    (illegalSubTree, errorReason) <- illegalFunc operatorsIllegalSide
    return (node illegalSubTree (legalShape Or (amount - 1 - operatorsIllegalSide)), errorReason)

genIllegalOperator :: (Int -> SynTree BinOp ()) -> [BinOp] -> Int -> Gen (SynTree BinOp (), ErrorReason)
genIllegalOperator recF operators restOperators =
    do
        errorOperator <- elements operators
        leftOperators <- choose (0, restOperators - 1)
        return (Binary errorOperator (recF leftOperators) (recF (restOperators - 1 - leftOperators)), IllegalOperator)



legalShape :: BinOp -> Int -> SynTree BinOp ()
legalShape operator amount = foldr (Binary operator . Leaf) (Leaf ()) (replicate amount ())
