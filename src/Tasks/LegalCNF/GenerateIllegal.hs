
module Tasks.LegalCNF.GenerateIllegal (
    genIllegalSynTree
    ) where


import qualified Formula.Types as SetFormula hiding (Dnf(..), Con(..))

import Control.Monad (join)
import Data.List ((\\))
import Data.Set (size, toList)
import Test.QuickCheck (Gen, choose, elements, frequency)
import Test.QuickCheck.Gen (oneof)

import Formula.Types (Clause(Clause))
import Tasks.LegalCNF.GenerateLegal (genClause, genCnf)
import Trees.Helpers (clauseToSynTree, collectLeaves, literalToSynTree, relabelShape)
import Trees.Types (BinOp(..), SynTree(..), allBinaryOperators)



genIllegalSynTree :: (Int,Int) -> (Int,Int) -> [Char] -> Bool -> Gen (SynTree BinOp Char)
genIllegalSynTree
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
            firstSyntaxShape <- genIllegalCNFShape allowArrowOperators (clauses - 1)
            clauseList <- toList . SetFormula.clauseSet
              <$> genCnf (clauses, clauses) (minClauseLength, maxClauseLength) usedLiterals
            return (genIllegalCNF firstSyntaxShape clauseList)
        else do
            clauses <- choose (minClauseAmount, maxClauseAmount)
            genCNFWithOneIllegalClause (minClauseLength, maxClauseLength) usedLiterals (clauses - 1) allowArrowOperators



genCNFWithOneIllegalClause :: (Int,Int) -> [Char] -> Int -> Bool -> Gen (SynTree BinOp Char)
genCNFWithOneIllegalClause (minClauseLength, maxClauseLength) usedLiterals ands allowArrowOperators = do
        clauseList <- toList . SetFormula.clauseSet <$>
          genCnf (ands, ands) (minClauseLength, maxClauseLength) usedLiterals
        illegalTree <- illegalClauseTree (minClauseLength, maxClauseLength) usedLiterals allowArrowOperators
        let illLength = length (collectLeaves illegalTree)
            (first, second) = span (\(Clause clause) -> illLength >= size clause) clauseList
            headTrees = map clauseToSynTree first
            tailTrees = map clauseToSynTree second
        return (foldr1 (Binary And) (headTrees ++ (illegalTree : tailTrees)))



genIllegalCNF :: SynTree BinOp () -> [SetFormula.Clause] -> SynTree BinOp Char
genIllegalCNF treeShape = join . relabelShape treeShape . map clauseToSynTree



illegalClauseTree :: (Int,Int) -> [Char] -> Bool -> Gen (SynTree BinOp Char)
illegalClauseTree (minClauseLength, maxClauseLength) usedLiterals allowArrowOperators = do
    treeLength <- choose (max 2 minClauseLength, maxClauseLength)
    illegalSynTreeShape <- genIllegalClauseShape True allowArrowOperators (treeLength - 1)
    leaves <- toList . SetFormula.literalSet <$> genClause (treeLength,treeLength) usedLiterals
    return (relabelShape illegalSynTreeShape leaves >>= literalToSynTree)



genIllegalShapeInSubTree :: Int -> (Int -> Gen (SynTree BinOp ())) -> BinOp -> Gen (SynTree BinOp ())
genIllegalShapeInSubTree amount illegalFunc operator = do
    operatorsIllegalSide <- choose (1, amount - 1)
    node <- elements [Binary operator, flip (Binary operator)]
    illegalSubTree <- illegalFunc operatorsIllegalSide
    return (node illegalSubTree (legalShape Or (amount - 1 - operatorsIllegalSide)))



genIllegalClauseShape :: Bool -> Bool -> Int -> Gen (SynTree BinOp ())
genIllegalClauseShape _ _ 0 = error "impossible"
genIllegalClauseShape ifFirstLayer allowArrowOperators ors = do
    ifUseError <- frequency [(1, return True), (ors - 1, return False)]
    if ifUseError
    then  if allowArrowOperators
          then oneof [ return (Not (legalShape Or ors))
                     , genIllegalOperator (legalShape Or) (Equi : Impl : BackImpl : [And | not ifFirstLayer]) ors
                     ]
          else  if ifFirstLayer
                then return (Not (legalShape Or ors))
                else oneof [ return (Not (legalShape Or ors))
                           , genIllegalOperator (legalShape Or) [And] ors
                           ]
    else genIllegalShapeInSubTree ors (genIllegalClauseShape False allowArrowOperators) Or



genIllegalCNFShape :: Bool -> Int -> Gen (SynTree BinOp ())
genIllegalCNFShape _ 0 = error "impossible"
genIllegalCNFShape True 1 = oneof [ return (Not (legalShape And 1))
                                  , genIllegalOperator (legalShape And) (allBinaryOperators \\ [And, Or]) 1
                                  ]
genIllegalCNFShape False 1 = return (Not (legalShape And 1))
genIllegalCNFShape allowArrowOperators ands = do
    ifUseError <- frequency[(1, return True), (ands - 1, return False)]
    if ifUseError
    then oneof [ return (Not (legalShape And ands))
               , genIllegalOperator (legalShape And)
                   (if allowArrowOperators then allBinaryOperators \\ [And] else [Or]) ands
               ]
    else genIllegalShapeInSubTree ands (genIllegalCNFShape allowArrowOperators) And



genIllegalOperator :: (Int -> SynTree BinOp ()) -> [BinOp] -> Int -> Gen (SynTree BinOp ())
genIllegalOperator recF operators restOperators =
    do
        errorOperator <- elements operators
        leftOperators <- choose (0, restOperators - 1)
        return (Binary errorOperator (recF leftOperators) (recF (restOperators - 1 - leftOperators)))



legalShape :: BinOp -> Int -> SynTree BinOp ()
legalShape operator amount = foldr (Binary operator . Leaf) (Leaf ()) (replicate amount ())
