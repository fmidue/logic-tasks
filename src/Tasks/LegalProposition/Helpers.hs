{-# LANGUAGE RecordWildCards #-}
module Tasks.LegalProposition.Helpers where
import Tasks.SynTree.Config (SynTreeConfig (..))
import qualified Data.Map as Map (filter)

formulaAmount :: SynTreeConfig -> Integer
formulaAmount SynTreeConfig{..} =
  let availableOperators = Map.filter (> 0) binOpFrequencies
    in fromIntegral (length availableOperators) ^ ((maxNodes + 1) `div` 2 - minUniqueBinOperators)
