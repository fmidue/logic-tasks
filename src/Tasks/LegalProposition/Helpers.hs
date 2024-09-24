{-# LANGUAGE RecordWildCards #-}
module Tasks.LegalProposition.Helpers where
import Tasks.SynTree.Config (SynTreeConfig (..), arrowOperatorsAllowed)
import Trees.Types (BinOp)

formulaAmount :: SynTreeConfig -> Integer
formulaAmount cfg@SynTreeConfig{..} =
  (if arrowOperatorsAllowed cfg then fromIntegral (length [minBound..maxBound :: BinOp]) else 2)
    ^ ((maxNodes + 1) `div` 2 - minUniqueBinOperators)
