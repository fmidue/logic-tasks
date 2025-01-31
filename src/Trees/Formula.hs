{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Trees.Formula where
import Formula.Types (Formula(..))
import Trees.Types (SynTree(..), BinOp(..))
import qualified Formula.Types as F (Literal(..))
import Data.List (find)
import Data.List.Extra (nubSort)
import Trees.Helpers (collectLeaves, treeNodes)

instance Formula (SynTree BinOp Char) where
  literals (Leaf x) = [F.Pos x]
  literals (Not (Leaf x)) = [F.Neg x]
  literals (Not x) = literals x
  literals (Binary _ l r) = nubSort $ literals l ++ literals r

  atomics = nubSort . collectLeaves

  amount = fromIntegral . treeNodes

  evaluate allocation (Leaf x) = snd <$> find (\(k,_) -> x == k) allocation
  evaluate allocation (Not x) = not <$> evaluate allocation x
  evaluate allocation (Binary op l r) = applyMaybe (
    case op of
      And -> (&&)
      Or -> (||)
      Impl -> \x y -> not x || y
      BackImpl -> \x y -> x || not y
      Equi -> (==)
    ) (evaluate allocation l) (evaluate allocation r)
    where
      applyMaybe _ Nothing _ = Nothing
      applyMaybe _ _ Nothing = Nothing
      applyMaybe f (Just x) (Just y) = Just $ f x y

