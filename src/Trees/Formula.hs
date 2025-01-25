{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Trees.Formula where
import Formula.Types (Formula(..))
import Trees.Types (SynTree(..), BinOp(..))
import qualified Formula.Types as F (Literal(..))
import Data.List (find)
import Data.List.Extra (nubSort)
import Trees.Helpers (collectLeaves, treeNodes, replaceSubFormula)
import Formula.Util (isSemanticEqual)

instance Formula (SynTree BinOp Char) where
  literals (Leaf x) = [F.Literal x]
  literals (Not (Leaf x)) = [F.Not x]
  literals (Not x) = literals x
  literals (Binary _ l r) = nubSort $ literals l ++ literals r

  atomics :: SynTree BinOp Char -> [F.Literal]
  atomics = map F.Literal . nubSort . collectLeaves

  amount = fromIntegral . treeNodes

  evaluate allocation (Leaf x) = snd <$> find (\(k,_) -> F.Literal x == k) allocation
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

hasUnusedAtoms :: SynTree BinOp Char -> Bool
hasUnusedAtoms t = any (\(F.Literal c) -> isSemanticEqual t (replaceSubFormula (Leaf c) (Not (Leaf c)) t)) atoms
  where atoms = atomics t
