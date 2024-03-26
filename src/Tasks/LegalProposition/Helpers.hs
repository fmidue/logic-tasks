{-# LANGUAGE RecordWildCards #-}
module Tasks.LegalProposition.Helpers where
import Tasks.SynTree.Config (SynTreeConfig (..))
import Trees.Types (BinOp)

formulaAmount :: SynTreeConfig -> Int
formulaAmount SynTreeConfig{..} =
  (if allowArrowOperators then length [minBound..maxBound :: BinOp] else 2)
    ^ ((maxNodes - 1) `div` 2 - minUniqueBinOperators + 1)
