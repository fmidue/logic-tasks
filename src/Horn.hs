module Horn where

import Data.Char (toLower)
import Data.Containers.ListUtils (nubOrd)
import Test.QuickCheck.Gen

import Trees.Types (BinOp(..), SynTree(..))
import Trees.Helpers (collectLeaves)

type Protocol = (Int, [(Int, Char)]) --sollte nur [(Int,Char)] sein oder [(Int,[Char])] vllt besser das Zweite

v1, v2 :: [SynTree BinOp Char]
v1 =
  [ Binary Impl (Leaf 'B') (Leaf 'A')
  , Binary Impl (Leaf '1') (Leaf 'B')
  , Binary Impl (Leaf 'C') (Leaf 'A')
  , Binary Impl (Leaf 'C') (Leaf '0')
  ]
v2 =
  [ Binary Impl (Leaf 'A') (Leaf 'B')
  , Binary Impl (Leaf '1') (Leaf 'A')
  , Binary Impl (Binary And (Leaf 'B') (Leaf 'A')) (Leaf '0')
  , Binary Impl (Leaf 'D') (Leaf '0')
  ]

makeHornFormula :: [SynTree BinOp Char] -> Int -> Gen (SynTree BinOp Char)
makeHornFormula spirit extra = do
    permutation <- shuffle spirit
    let withAdded = concatMap addClause $ zip (take extra permutation) ['M'..]
    clauses <- shuffle (withAdded ++ drop extra permutation)
    let formula =  toLower <$> foldr1 (Binary And) clauses
    atomics <- shuffle (getAllAtomics formula)
    return (foldl (flip (uncurry replace)) formula (zip atomics ['A'..]))
  where
    addClause (Binary Impl a b, x) = [Binary Impl a (Leaf x), Binary Impl (Leaf x) b]
    addClause _ = []

isHornFormulaI :: SynTree BinOp c -> Bool
isHornFormulaI =  all isHornClauseI . getClauses

isHornClauseI :: SynTree BinOp c -> Bool
isHornClauseI (Binary Impl a (Leaf _)) = case a of
    Leaf _ -> True
    (Binary And x y) -> isConj x && isConj y
    _ -> False
  where
    isConj (Leaf _) = True
    isConj (Binary And x y) = isConj x && isConj y
    isConj _ = False
isHornClauseI _ = False

getAllAtomics :: SynTree BinOp Char -> [Char]
getAllAtomics tree = nubOrd $ filter (`notElem` ['0','1']) (collectLeaves tree)

modelFromSolution :: (Bool, Protocol) -> [Char] -> [(Char, Bool)] -- doof, dass hier auch wieder die Formel verarbeitet werden muss, sollte in der Solution direkt vorkommen
modelFromSolution (False,_) _ = []
modelFromSolution (True,(_,marked)) cs = map (\(_,a) -> (a,True)) marked ++
    map (,False) (filter (`notElem` map snd marked) cs)

getClauses :: SynTree BinOp c -> [SynTree BinOp c]
getClauses (Binary And leftPart rightPart) = getClauses leftPart ++ getClauses rightPart
getClauses formula = [formula]

isFact :: SynTree BinOp Char -> Bool
isFact (Binary Impl (Leaf '1') (Leaf _)) = True
isFact _ = False

charFromFact :: SynTree BinOp Char -> Char
charFromFact (Binary Impl (Leaf '1') (Leaf a)) = a
charFromFact _ = error "Cannot get Char from not a fact."

getFacts :: [SynTree BinOp Char] -> [Char]
getFacts = map charFromFact . filter isFact

findSolution :: SynTree BinOp Char -> (Bool, Protocol)
findSolution formula = startAlg (doStep allClauses) (startProtocol facts)
  where
    facts = getFacts allClauses
    allClauses = getClauses formula

    startProtocol :: [Char] -> Protocol
    startProtocol cs = (1, map (1,) cs) -- der aktuelle Schritt kann auch aus dem letzten Tupel entnommen werden

    addStep :: Char -> Protocol -> Protocol
    addStep c (step, record) = (step+1, record ++ [(step+1,c)])

    startAlg :: Maybe [SynTree BinOp Char] -> Protocol -> (Bool, Protocol)
    startAlg Nothing protocol = (False, protocol)
    startAlg (Just []) protocol = (True, protocol)
    startAlg (Just cs) protocol =
      let updatedProtocol = case toBeMarked cs of Nothing -> protocol
                                                  Just '0' -> protocol
                                                  Just c  -> addStep c protocol
      in startAlg (doStep cs) updatedProtocol

toBeMarked :: [SynTree BinOp Char] -> Maybe Char
toBeMarked clauses = case getFacts clauses of
  []    -> Nothing
  (x:_) -> Just x

doStep :: [SynTree BinOp Char] -> Maybe [SynTree BinOp Char]
doStep clauses = case toBeMarked clauses of
  Nothing  -> Just []
  Just '0' -> Nothing
  Just a   -> Just $ simplify $ map (replace a '1') clauses

replace :: Eq a => a -> a -> SynTree BinOp a -> SynTree BinOp a
replace x y = fmap (\a -> if a == x then y else a)

simplify :: [SynTree BinOp Char] -> [SynTree BinOp Char]
simplify clauses = if appliedOnce == appliedTwice then appliedOnce else simplify appliedTwice
  where
    appliedOnce = concatMap removeOnes clauses
    appliedTwice = concatMap removeOnes appliedOnce

removeOnes :: SynTree BinOp Char -> [SynTree BinOp Char]
removeOnes tree = case tree of
    Binary Impl (Leaf '1') (Leaf '1')                -> []
    Binary Impl (Binary And (Leaf '1') (Leaf '1')) b -> [Binary Impl (Leaf '1') b]
    Binary Impl (Binary And (Leaf '1') a) b          -> [Binary Impl a b]
    Binary Impl (Binary And a (Leaf '1')) b          -> [Binary Impl a b]
    _                                                -> [tree]
