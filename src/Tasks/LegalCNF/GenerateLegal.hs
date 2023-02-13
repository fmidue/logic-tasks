module Tasks.LegalCNF.GenerateLegal (
  genClause,
  genCnf
) where

import Test.QuickCheck (choose, Gen, suchThat, elements)
import Test.QuickCheck.Gen (vectorOf)
import qualified Formula.Types as Setform
import Data.Set (fromList)
import Auxiliary (listNoDuplicate)

genLiteral :: [Char] -> Gen Setform.Literal
genLiteral lits = do
   rChar <- elements lits
   elements [Setform.Literal rChar, Setform.Not rChar]


genClause :: (Int,Int) -> [Char] -> Gen Setform.Clause
genClause (minClauseLength, maxClauseLength) usedLiterals = do
    literals <- choose (minClauseLength, maxClauseLength)
    clause <- vectorOf literals (genLiteral usedLiterals) `suchThat` listNoDuplicate
    return (Setform.Clause (fromList clause))

genCnf :: (Int,Int) -> (Int,Int) -> [Char] -> Gen Setform.Cnf
genCnf (minClauseAmount, maxClauseAmount) (minClauseLength, maxClauseLength) usedLiterals = do
  clauses <- choose (minClauseAmount, maxClauseAmount)
  cnf <- vectorOf clauses (genClause (minClauseLength, maxClauseLength) usedLiterals) `suchThat` listNoDuplicate
  return (Setform.Cnf (fromList cnf))
