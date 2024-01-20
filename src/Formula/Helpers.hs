{-# LANGUAGE RecordWildCards #-}
module Formula.Helpers where
import Formula.Types (PrologLiteral (..), PrologClause(..), terms, ClauseShape(AnyClause, HornClause), HornShape (..))

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
