module Horn where

import Data.Char (toLower)
import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (isJust)
import Data.List.Extra (notNull)
import Test.QuickCheck.Gen

import Trees.Types (BinOp(..), SynTree(..))
import Trees.Helpers (collectLeaves)


type Protocol = [(Int,[Char])]
type Allocation = [(Char,Bool)]


v1, v2, v3 :: [SynTree BinOp Char]
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
v3 =
  [ Binary Impl (Leaf '1') (Leaf 'A')
  , Binary Impl (Leaf '1') (Leaf 'B')
  , Binary Impl (Leaf 'A') (Leaf 'C')
  , Binary Impl (Leaf 'B') (Leaf 'D')
  ]

makeHornFormula :: [SynTree BinOp Char] -> Int -> Gen (SynTree BinOp Char)
makeHornFormula spirit extra = do
    permutation <- shuffle spirit
    let withAdded = concatMap addClause $ zip (take extra permutation) ['M'..'Z']
    clauses <- shuffle (withAdded ++ drop extra permutation)
    let lowerCaseClauses = map (fmap toLower) clauses
    let formula = foldr1 (Binary And) lowerCaseClauses
    atomics <- shuffle (getAllAtomics lowerCaseClauses)
    return (foldl (flip (uncurry replace)) formula (zip atomics ['A'..'Z']))
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

getAllAtomics :: [SynTree BinOp Char] -> [Char]
getAllAtomics clauses = nubOrd $ concatMap (filter (`notElem` ['0', '1']) . collectLeaves) clauses

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


startAlgorithm :: SynTree BinOp Char -> (Protocol,Bool,Allocation)
startAlgorithm formula = markingAlg modifiedClauses [(1, facts) | notNull facts]
  where
    facts = getFacts clauses
    clauses = getClauses formula
    modifiedClauses = foldl doStep clauses facts

markingAlg :: [SynTree BinOp Char] -> Protocol -> (Protocol,Bool,Allocation)
markingAlg clauses protocol = case nextToMark clauses of
    Nothing   -> (protocol, True, model)
    Just '0'  -> (protocol, False, [])
    Just fact -> markingAlg (doStep clauses fact) (addStep fact protocol)
  where
    trueAtoms = concatMap (\(_,cs) -> concatMap (\c -> [(c,True)]) cs) protocol
    model = trueAtoms ++
        concatMap (\c -> ([(c, False) | c `notElem` map fst trueAtoms])) (getAllAtomics clauses)


addStep :: Char -> Protocol -> Protocol
addStep marked protocol = protocol ++ [(step,[marked])]
  where
    step = (\(prevStep,_) -> prevStep + 1) $ last protocol

nextToMark :: [SynTree BinOp Char] -> Maybe Char
nextToMark clauses = case getFacts clauses of
    []    -> Nothing
    (c:_) -> Just c

doStep :: [SynTree BinOp Char] -> Char -> [SynTree BinOp Char]
doStep clauses fact = simplify $ map (replace fact '1') clauses

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

checkStepOrder :: [SynTree BinOp Char] -> [Char] -> Bool
checkStepOrder clauses marked = isJust $ foldl tryStep (Just clauses) marked

tryStep :: Maybe [SynTree BinOp Char] -> Char -> Maybe [SynTree BinOp Char]
tryStep (Just clauses) c = if Binary Impl (Leaf '1') (Leaf c) `elem` clauses
    then Just (doStep clauses c)
    else Nothing
tryStep Nothing        _ = Nothing
