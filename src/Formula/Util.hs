
module Formula.Util
       (
         isPositive
       , mkClause
       , mkCon
       , isEmptyClause
       , mkCnf
       , mkDnf
       , mkPrologClause
       , isEmptyCnf
       , hasEmptyClause
       , isEmptyDnf
       , hasEmptyCon
       , xorSat
       , andSat
       , sat
       , transformProlog
       , flipPol
       ) where


import qualified Data.Set as Set
import qualified SAT.MiniSat as Sat

import Data.Maybe(fromJust)

import Formula.Types


---------------------------------------------------------------------------------------------------

-- | Is the input a positive literal?
isPositive :: Literal -> Bool
isPositive (Not _) = False
isPositive _ = True

---------------------------------------------------------------------------------------------------

-- | Builds a clause containing the given literals.
mkClause :: [Literal] -> Clause
mkClause xs = Clause $ Set.fromList xs


-- | Is the input the empty clause?
isEmptyClause :: Clause -> Bool
isEmptyClause (Clause set) = Set.null set

---------------------------------------------------------------------------------------------------

-- | Builds a formula in cnf containing the given clauses.
mkCnf :: [Clause] -> Cnf
mkCnf xs = Cnf $ Set.fromList xs


-- | Is the input the empty conjunction?
isEmptyCnf :: Cnf -> Bool
isEmptyCnf (Cnf set) = Set.null set


-- | Does the cnf contain an empty clause?
hasEmptyClause :: Cnf -> Bool
hasEmptyClause (Cnf set) = Clause Set.empty `Set.member` set



---------------------------------------------------------------------------------------------------


-- | Builds a conjunction containing the given literals.
mkCon :: [Literal] -> Con
mkCon xs = Con $ Set.fromList xs


-- | Builds a formula in dnf containing the given conjunctions.
mkDnf :: [Con] -> Dnf
mkDnf xs = Dnf $ Set.fromList xs


-- | Is the input the empty clause?
isEmptyDnf :: Dnf -> Bool
isEmptyDnf (Dnf set) = Set.null set


-- | Does the dnf contain an empty conjunction?
hasEmptyCon :: Dnf -> Bool
hasEmptyCon (Dnf set) = Con Set.empty `Set.member` set




---------------------------------------------------------------------------------------------------


logOpSat :: (Formula a, Formula b)
         => (Sat.Formula Char -> Sat.Formula Char -> Sat.Formula Char)
         -> a
         -> b
         -> Bool
logOpSat op f1 f2 = Sat.satisfiable (op (convert f1) (convert f2))



-- | (f1 ``xorSat`` f2) indicates whether (f1 XOR f2) is satisfiable
xorSat :: (Formula a, Formula b) => a -> b -> Bool
xorSat = logOpSat (Sat.:++:)


-- | (f1 ``andSat`` f2) indicates whether (f1 /\\ f2) is satisfiable
andSat :: (Formula a, Formula b) => a -> b -> Bool
andSat = logOpSat (Sat.:&&:)



-- | Indicates whether the given formula is satisfiable
sat :: Formula a => a -> Bool
sat f = Sat.satisfiable $ convert f


----------------------------------------------------------------------------------------------------------


flipPol :: PrologLiteral -> PrologLiteral
flipPol (PrologLiteral b n f) = PrologLiteral (not b) n f


mkPrologClause :: [PrologLiteral] -> PrologClause
mkPrologClause ls = PrologClause (Set.fromList ls)


transformProlog :: PrologClause -> [(PrologLiteral,Literal)] -> Clause
transformProlog pc mapping = mkClause $ map (fromJust . (`lookup` mapping)) lits
  where
    lits = Set.toList (pliterals pc)



