{-# LANGUAGE RecordWildCards #-}
module Formula.Helpers where
import Formula.Types (PrologLiteral (..), PrologClause(..), terms)

isHornClause :: PrologClause -> Bool
isHornClause clause = length (filter (\PrologLiteral {..} -> polarity) (terms clause)) <= 1
