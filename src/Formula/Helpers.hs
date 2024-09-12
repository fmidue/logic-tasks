{-# LANGUAGE RecordWildCards #-}
module Formula.Helpers where
import Formula.Types (
  PrologLiteral (..),
  PrologClause(..),
  ClauseShape(AnyClause, HornClause),
  HornShape (..),
  Clause(..),
  Cnf(..),
  terms
  )
import Data.Set (toList)
import Data.List (intercalate)

hasTheClauseShape :: ClauseShape -> PrologClause -> Bool
hasTheClauseShape AnyClause _ = True
hasTheClauseShape (HornClause hornShape) clause =
  let positiveLiteralCount = length $ filter (\PrologLiteral {..} -> polarity) (terms clause)
      negativeLiteralCount = length (terms clause) - positiveLiteralCount
      in case hornShape of
        AnyHornClause -> positiveLiteralCount <= 1
        Fact -> positiveLiteralCount == 1 && negativeLiteralCount == 0
        Procedure -> positiveLiteralCount == 1 && negativeLiteralCount > 0
        Query -> positiveLiteralCount == 0 && negativeLiteralCount > 0

showClauseAsSet :: Clause -> String
showClauseAsSet Clause{..}
  | null literalSet = "{ }"
  | otherwise = "{ " ++ intercalate ", " (map show (toList literalSet)) ++ " }"

showCnfAsSet :: Cnf -> String
showCnfAsSet Cnf{..}
  | null clauseSet = "{ }"
  | otherwise = "{ " ++ intercalate ", " (map showClauseAsSet (toList clauseSet)) ++ " }"
