
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
       , isSemanticEqual
       -- , isSemanticEqualSat
       , cnfDependsOnAllAtomics
       , dnfDependsOnAllAtomics
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

-- | Return the used char in an atom. Can be removed after #248 was merged.
atomicChar :: Literal -> Char
atomicChar (Literal c) = c
atomicChar _ = error "this should not happen"

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

cnfDependsOnAllAtomics :: Cnf -> Bool
cnfDependsOnAllAtomics cnf = not $ any (\c -> isSemanticEqual cnf (replaceAtomInCnf c cnf) ) atoms
  where atoms = map atomicChar $ atomics cnf

        replaceAtomInCnf c (Cnf clauses) = Cnf $ Set.map (replaceAtomInClause c) clauses

        replaceAtomInClause c (Clause lits) = Clause $ Set.map (replaceLiteral c) lits

        replaceLiteral c l@(Literal a)
          | a == c = Not c
          | otherwise = l
        replaceLiteral c l@(Not a)
          | a == c = Literal c
          | otherwise = l


dnfDependsOnAllAtomics :: Dnf -> Bool
dnfDependsOnAllAtomics dnf = not $ any (\c -> isSemanticEqual dnf (replaceAtomInDnf c dnf) ) atoms
  where atoms = map atomicChar $ atomics dnf

        replaceAtomInDnf c (Dnf cons) = Dnf $ Set.map (replaceAtomInCon c) cons

        replaceAtomInCon c (Con lits) = Con $ Set.map (replaceLiteral c) lits

        replaceLiteral c l@(Literal a)
          | a == c = Not c
          | otherwise = l
        replaceLiteral c l@(Not a)
          | a == c = Literal c
          | otherwise = l


---------------------------------------------------------------------------------------------------


logOpSat :: (ToSAT a, ToSAT b)
         => (Sat.Formula Char -> Sat.Formula Char -> Sat.Formula Char)
         -> a
         -> b
         -> Bool
logOpSat op f1 f2 = Sat.satisfiable (op (convert f1) (convert f2))



-- | (f1 ``xorSat`` f2) indicates whether (f1 XOR f2) is satisfiable
xorSat :: (ToSAT a, ToSAT b) => a -> b -> Bool
xorSat = logOpSat (Sat.:++:)


-- | (f1 ``andSat`` f2) indicates whether (f1 /\\ f2) is satisfiable
andSat :: (ToSAT a, ToSAT b) => a -> b -> Bool
andSat = logOpSat (Sat.:&&:)

-- | Indicates whether the given formula is satisfiable
sat :: ToSAT a => a -> Bool
sat f = Sat.satisfiable $ convert f

-- | Are two formulas semantically equal?
isSemanticEqual :: ToSAT a => a -> a -> Bool
isSemanticEqual a b = not $ xorSat a b

----------------------------------------------------------------------------------------------------------


flipPol :: PrologLiteral -> PrologLiteral
flipPol (PrologLiteral b n f) = PrologLiteral (not b) n f


mkPrologClause :: [PrologLiteral] -> PrologClause
mkPrologClause ls = PrologClause (Set.fromList ls)


transformProlog :: PrologClause -> [(PrologLiteral,Literal)] -> Clause
transformProlog pc mapping = mkClause $ map (fromJust . (`lookup` mapping)) lits
  where
    lits = Set.toList (pLiterals pc)
