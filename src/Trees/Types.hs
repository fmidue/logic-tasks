{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}

module Trees.Types
    (
    SynTree(..),
    BinOp(..),
    PropFormula(..),
    showOperator,
    showOperatorNot,
    allBinaryOperators,
    ) where


import GHC.Generics
import Test.QuickCheck



data BinOp = And | Or | Impl | Equi
  deriving (Eq, Generic, Ord, Show, Enum, Bounded)

showOperator :: BinOp -> String
showOperator And = "/\\"
showOperator Or = "\\/"
showOperator Impl = "=>"
showOperator Equi = "<=>"

allBinaryOperators :: [BinOp]
allBinaryOperators = [minBound .. maxBound]

showOperatorNot :: String
showOperatorNot = "~"

data SynTree o c
    = Binary o (SynTree o c) (SynTree o c)
    | Not (SynTree o c)
    | Leaf c
  deriving (Eq, Generic, Ord, Show, Functor, Foldable, Traversable)

instance Applicative (SynTree o) where
  pure = Leaf
  m1 <*> m2 = m1 >>= \f -> m2 >>= \x -> return (f x)

instance Monad (SynTree o) where
  Binary oper a b >>= k = Binary oper (a >>= k) (b >>= k)
  Not a           >>= k = Not (a >>= k)
  Leaf a          >>= k = k a


data PropFormula = Atomic Char | Neg PropFormula | Brackets PropFormula | Assoc BinOp PropFormula PropFormula
  deriving (Eq)


instance Show PropFormula where
  show (Atomic c) = [c]
  show (Neg f) = showOperatorNot ++ show f
  show (Brackets f) = '(' : show f ++ ")"
  show (Assoc o f1 f2) = show f1 ++ " " ++ showOperator o ++ " " ++ show f2


instance Arbitrary PropFormula where
   arbitrary = sized pf
     where
       pf :: Int -> Gen PropFormula
       pf 0 = Atomic <$> elements ['A'..'Z']
       pf n = oneof [Neg <$> next, Brackets <$> next, Assoc <$> elements allBinaryOperators <*> next <*> next]
         where next = pf (n-1)

