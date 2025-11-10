{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Trees.Types
    (
    SynTree(..),
    LayeredSynTree(..),
    BinOp(..),
    FormulaAnswer(..),
    PropFormula(..),
    TreeFormulaAnswer(..),
    showOperator,
    showOperatorNot,
    allBinaryOperators,
    toSynTree,
    toLayeredTree,
    validateLayers,
    unfoldLayers,
    ) where


import GHC.Generics
import Formula.Types (ToSAT(..))
import qualified SAT.MiniSat as Sat
import Data.Data (Data)
import Data.Tuple (swap)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty


data BinOp = And | Or | Impl | BackImpl | Equi
  deriving (Eq, Data, Generic, Ord, Show, Enum, Bounded)

showOperator :: BinOp -> String
showOperator And = "∧"
showOperator Or = "∨"
showOperator Impl = "=>"
showOperator BackImpl = "<="
showOperator Equi = "<=>"

allBinaryOperators :: [BinOp]
allBinaryOperators = [minBound .. maxBound]

showOperatorNot :: String
showOperatorNot = "¬"

data SynTree o c
    = Binary o (SynTree o c) (SynTree o c)
    | Not (SynTree o c)
    | Leaf c
  deriving (Data, Eq, Generic, Ord, Show, Functor, Foldable, Traversable)

instance Applicative (SynTree o) where
  pure = Leaf
  m1 <*> m2 = m1 >>= \f -> m2 >>= \x -> return (f x)

instance Monad (SynTree o) where
  Binary operator a b >>= k = Binary operator (a >>= k) (b >>= k)
  Not a               >>= k = Not (a >>= k)
  Leaf a              >>= k = k a

instance ToSAT (SynTree BinOp Char) where
  convert (Leaf a) = Sat.Var a
  convert (Not f) = Sat.Not (convert f)
  convert (Binary op l r) = convertBinOp op leftFormula rightFormula
    where (leftFormula, rightFormula) = (if op == BackImpl then swap else id) (convert l, convert r)
          convertBinOp And = (Sat.:&&:)
          convertBinOp Or = (Sat.:||:)
          convertBinOp Impl = (Sat.:->:)
          convertBinOp BackImpl = (Sat.:->:)
          convertBinOp Equi = (Sat.:<->:)

newtype TreeFormulaAnswer = TreeFormulaAnswer {maybeTree :: Maybe (SynTree BinOp Char)} deriving (Ord, Eq, Generic)



instance Show TreeFormulaAnswer where
  show (TreeFormulaAnswer (Just p)) = show p
  show _ = ""


data PropFormula c
    = Atomic c
    | Neg (PropFormula c)
    | Brackets (PropFormula c)
    | Assoc BinOp (PropFormula c) (PropFormula c)
  deriving (Data, Eq, Ord, Foldable)


instance Show (PropFormula Char) where
  show (Atomic c) = [c]
  show (Neg f) = showOperatorNot ++ show f
  show (Brackets f) = '(' : show f ++ ")"
  show (Assoc o f1 f2) = show f1 ++ " " ++ showOperator o ++ " " ++ show f2


instance ToSAT (PropFormula Char) where
  convert = convert . toSynTree


toSynTree :: PropFormula a -> SynTree BinOp a
toSynTree (Atomic c) = Leaf c
toSynTree (Neg p) = Not $ toSynTree p
toSynTree (Brackets p) = toSynTree p
toSynTree (Assoc op l r) = Binary op (toSynTree l) (toSynTree r)

newtype FormulaAnswer = FormulaAnswer {maybeForm :: Maybe (PropFormula Char)} deriving (Eq, Generic)

instance Show FormulaAnswer where
  show (FormulaAnswer (Just p)) = show p
  show _ = ""

data LayeredSynTree o c
    = LTreeLayer (LayeredSynTree o c) (NonEmpty (o,LayeredSynTree o c))
    | LTreeNot (LayeredSynTree o c)
    | LTreeLeaf c
  deriving (Data, Eq, Generic, Ord, Show, Functor, Foldable, Traversable)

toLayeredTree :: PropFormula a -> LayeredSynTree BinOp a
toLayeredTree (Atomic c) = LTreeLeaf c
toLayeredTree (Neg p) = LTreeNot $ toLayeredTree p
toLayeredTree (Brackets p) = toLayeredTree p
toLayeredTree x@Assoc{} =
  case gatherLayer x of
    (_ :| [],[]) -> error "impossible"
    (f :| fs,ops) -> LTreeLayer (toLayeredTree f) (NonEmpty.fromList $ zipWith (\o p -> (o, toLayeredTree p)) ops fs)
  where
    -- invariant: length(first list) == length(second list)+1
    gatherLayer :: PropFormula a -> (NonEmpty (PropFormula a), [BinOp])
    gatherLayer p@Atomic{} = (NonEmpty.singleton p,[])
    gatherLayer (Neg Assoc{}) = error "should not happen"
    gatherLayer p@Neg{} = (NonEmpty.singleton p,[])
    gatherLayer p@Brackets{} = (NonEmpty.singleton p,[])
    gatherLayer (Assoc op l r) =
      let
        (fsl,osl) = gatherLayer l
        (fsr,osr) = gatherLayer r
      in (fsl <> fsr,osl <> [op] <> osr)

validateLayers :: Eq o => (NonEmpty o -> Maybe String) -> LayeredSynTree o a -> Either String (LayeredSynTree o a)
validateLayers _ (LTreeLeaf c) = Right $ LTreeLeaf c
validateLayers check (LTreeNot p) = LTreeNot <$> validateLayers check p
validateLayers check (LTreeLayer p qs) =
  case check (NonEmpty.map fst qs) of
    Nothing -> LTreeLayer
      <$> validateLayers check p
      <*> traverse (\(o,f) -> fmap (o,) (validateLayers check f)) qs
    Just feedback -> Left feedback

unfoldLayers :: LayeredSynTree o a -> SynTree o a
unfoldLayers (LTreeLeaf c) = Leaf c
unfoldLayers (LTreeNot p) = Not $ unfoldLayers p
unfoldLayers (LTreeLayer p qs) = foldl (\t (o,q) -> Binary o t (unfoldLayers q)) (unfoldLayers p) qs
