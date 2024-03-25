{-# LANGUAGE FlexibleContexts #-}

module Trees.Helpers
    (
    numberAllBinaryNodes,
    collectLeaves,
    relabelShape,
    treeNodes,
    treeDepth,
    allNotLeafSubTrees,
    minDepthForNodes,
    maxLeavesForNodes,
    maxNodesForDepth,
    noSameSubTree,
    sameAssociativeOperatorAdjacent,
    similarExist,
    consecutiveNegations,
    cnfToSynTree,
    clauseToSynTree,
    literalToSynTree,
    numOfOps,
    numOfOpsInFormula,
    numOfUniqueBinOpsInSynTree,
    binOp,
    bothKids,
    collectUniqueBinOpsInSynTree
    ) where

import Control.Monad (void)
import Control.Monad.State (get, put, runState, evalState)
import Data.Set(fromList, Set, toList)
import Data.List.Extra (nubBy, nubOrd)
import qualified Data.Foldable as Foldable (toList)
import qualified Formula.Types as SetFormula hiding (Dnf(..), Con(..))
import Trees.Types (SynTree(..), BinOp(..), PropFormula(..))
import Auxiliary (listNoDuplicate)

numberAllBinaryNodes :: SynTree o c -> SynTree (o, Integer) c
numberAllBinaryNodes = flip evalState 1 . go
    where
      go (Leaf c) = return (Leaf c)
      go (Not t) = Not <$> go t
      go (Binary o t1 t2) = do { l <- next; t1' <- go t1; t2' <- go t2; return (Binary (o,l) t1' t2') }
      next = do {current <- get; put (current + 1); return current}

collectLeaves :: Foldable t => t c -> [c]
collectLeaves = Foldable.toList

relabelShape :: SynTree o () -> [c] -> SynTree o c
relabelShape shape contents =
    let (tree, rest)
          = runState (traverse adorn shape) contents
    in
        if null rest
        then tree
        else error "This should not happen!"
    where
      adorn _ =
        do {current <- get; put (tail current); return (head current)}

getNotLeafSubTrees :: SynTree o c -> [SynTree o c]
getNotLeafSubTrees t@(Binary _ a b) = getNotLeafSubTrees a ++ (t : getNotLeafSubTrees b)
getNotLeafSubTrees (Leaf _) =  []
getNotLeafSubTrees t@(Not a) = t : getNotLeafSubTrees a

allNotLeafSubTrees :: (Ord o, Ord c) => SynTree o c -> Set (SynTree o c)
allNotLeafSubTrees a = fromList (getNotLeafSubTrees a)

noSameSubTree :: (Eq o, Ord o, Ord c) => SynTree o c -> Bool
noSameSubTree synTree =
  let
    treeList = getNotLeafSubTrees synTree
  in
    listNoDuplicate treeList

treeNodes :: SynTree o c -> Integer
treeNodes (Binary _ a b) = 1 + treeNodes a + treeNodes b
treeNodes (Leaf _) =  1
treeNodes (Not a) = 1 + treeNodes a

treeDepth :: SynTree o c -> Integer
treeDepth (Not a) = 1 + treeDepth a
treeDepth (Leaf _) = 1
treeDepth (Binary _ a b) = 1 + max (treeDepth a) (treeDepth b)

minDepthForNodes :: Integer -> Integer
minDepthForNodes nodes = ceiling (logBase 2 (fromIntegral (nodes + 1) :: Float))

maxNodesForDepth :: Integer -> Integer
maxNodesForDepth depth = 2 ^ depth - 1

maxLeavesForNodes :: Integer -> Integer
maxLeavesForNodes nodes = (nodes + 1) `div` 2

sameAssociativeOperatorAdjacent :: SynTree BinOp c -> Bool
sameAssociativeOperatorAdjacent (Leaf _) = False
sameAssociativeOperatorAdjacent (Not a) = sameAssociativeOperatorAdjacent a
sameAssociativeOperatorAdjacent (Binary operator a b) =
    checkNextOperator a operator ||
    checkNextOperator b operator ||
    sameAssociativeOperatorAdjacent a ||
    sameAssociativeOperatorAdjacent b

checkNextOperator :: SynTree BinOp c -> BinOp -> Bool
checkNextOperator (Binary And _ _) And = True
checkNextOperator (Binary Or _ _) Or = True
checkNextOperator _ _ = False

similarTree :: Eq o => SynTree o c -> SynTree o c -> Bool
similarTree t1 t2 = void t1 == void t2

similarExist :: Eq o => [SynTree o c] -> Bool
similarExist trees = length (nubBy similarTree trees) /= length trees

consecutiveNegations :: SynTree o c -> Integer
consecutiveNegations (Binary _ a b) = max (consecutiveNegations a) (consecutiveNegations b)
consecutiveNegations (Not a) = max (consecutiveNegations a) (1 + continueNot a)
consecutiveNegations (Leaf _)  = 0

continueNot :: SynTree o c -> Integer
continueNot (Not a) = 1 + continueNot a
continueNot _ = 0

cnfToSynTree :: SetFormula.Cnf -> SynTree BinOp Char
cnfToSynTree = foldr1 (Binary And) . map clauseToSynTree . toList . SetFormula.clauseSet

clauseToSynTree :: SetFormula.Clause -> SynTree BinOp Char
clauseToSynTree = foldr1 (Binary Or) . map literalToSynTree . toList . SetFormula.literalSet

literalToSynTree :: SetFormula.Literal -> SynTree o Char
literalToSynTree (SetFormula.Literal a) = Leaf a
literalToSynTree (SetFormula.Not a) = Not (Leaf a)


numOfOps :: SynTree o c -> Integer
numOfOps (Binary _ t1 t2)= 1 + numOfOps t1 + numOfOps t2
numOfOps (Not t) = numOfOps t
numOfOps _ = 0


numOfOpsInFormula :: PropFormula c -> Integer
numOfOpsInFormula (Atomic _) = 0
numOfOpsInFormula (Neg f) = numOfOpsInFormula f
numOfOpsInFormula (Brackets f) = numOfOpsInFormula f
numOfOpsInFormula (Assoc _ f1 f2) = 1 + numOfOpsInFormula f1 + numOfOpsInFormula f2

numOfUniqueBinOpsInSynTree :: SynTree BinOp c -> Integer
numOfUniqueBinOpsInSynTree = fromIntegral . length . collectUniqueBinOpsInSynTree

binOp :: SynTree BinOp a -> Maybe BinOp
binOp (Binary op _ _) = Just op
binOp _ = Nothing

bothKids :: SynTree o a -> (SynTree o a, SynTree o a)
bothKids (Binary _ l r) = (l, r)
bothKids _ = error "impossible to land here"

collectUniqueBinOpsInSynTree :: SynTree BinOp a -> [BinOp]
collectUniqueBinOpsInSynTree (Leaf _) = []
collectUniqueBinOpsInSynTree (Not x) = collectUniqueBinOpsInSynTree x
collectUniqueBinOpsInSynTree (Binary op l r) = nubOrd $ concat [
  [op],
  collectUniqueBinOpsInSynTree l,
  collectUniqueBinOpsInSynTree r]
