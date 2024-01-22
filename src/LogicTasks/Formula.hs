module LogicTasks.Formula
       (
         module Formula.Util
       , Literal(..)
       , Clause
       , Con
       , Cnf
       , Dnf
       , Allocation
       , TruthValue(..)
       , PrologLiteral
       , PrologClause
       , terms
       , getClauses
       , getConjunctions
       , Formula(..)
       , ToSAT(..)
       , genLiteral
       , genClause
       , genCon
       , genCnf
       , genDnf
       , opposite
       , possibleAllocations
       ) where


import Formula.Types
import Formula.Util
