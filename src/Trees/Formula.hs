{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Trees.Formula where
import Formula.Types (Formula(..))
import Trees.Types (SynTree(..), BinOp(..))
import qualified Formula.Types as F (Literal(..))
import Data.List (sort, find)
import Data.List.Extra (nubOrd)
import Trees.Helpers (collectLeaves, treeNodes)

instance Formula (SynTree BinOp Char) where
  literals (Leaf x) = [F.Literal x]
  literals (Not (Leaf x)) = [F.Not x]
  literals (Not x) = literals x
  literals (Binary _ l r) = sort $ nubOrd $ literals l ++ literals r

  atomics = sort . map F.Literal . nubOrd . collectLeaves

  amount = fromIntegral . treeNodes

  evaluate allocation (Leaf x) = snd <$> find (\(k,_) -> F.Literal x == k) allocation
  evaluate allocation (Not x) = not <$> evaluate allocation x
  evaluate allocation (Binary op l r) = applyMaybe (
    case op of
      And -> (&&)
      Or -> (||)
      Impl -> \x y -> not x || y
      Equi -> (==)
    ) (evaluate allocation l) (evaluate allocation r)
    where
      applyMaybe _ Nothing _ = Nothing
      applyMaybe _ _ Nothing = Nothing
      applyMaybe f (Just x) (Just y) = Just $ f x y

