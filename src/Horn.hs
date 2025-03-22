module Horn where

import Trees.Types (BinOp(..), SynTree(..))
import Trees.Helpers (collectLeaves)

type Protocol = (Int, [(Int, Char)])

makeHornformula :: SynTree BinOp Char
makeHornformula = foldr1 (Binary And) clauses
  where
    clauses =
      [ Binary Impl (Leaf 'A') (Leaf 'B')
      , Binary Impl (Leaf '1') (Leaf 'A')
      , Binary Impl (Binary And (Leaf 'A') (Leaf 'D')) (Leaf '0')
      ]

isHornformulaI :: SynTree BinOp c -> Bool
isHornformulaI =  all isHornclauseI . getClauses

isHornclauseI :: SynTree BinOp c -> Bool
isHornclauseI (Binary Impl a (Leaf _)) = case a of
    Leaf _ -> True
    (Binary And x y) -> isConj x && isConj y
    _ -> False
  where
    isConj (Leaf _) = True
    isConj (Binary And x y) = isConj x && isConj y
    isConj _ = False
isHornclauseI _ = False

modellFromSolution :: (Bool, Protocol)  -> [Char] -> Maybe [(Char, Bool)]
modellFromSolution (False,_) _ = Nothing
modellFromSolution (True,(_,marked)) cs = Just $ (map (\(_,a) -> (a,True)) marked) ++ (map (,False) $ filter (\a -> notElem a (map snd marked)) cs)

getClauses :: SynTree BinOp c -> [SynTree BinOp c]
getClauses (Binary And leftPart rightPart) = getClauses leftPart ++ getClauses rightPart
getClauses formula = [formula]

isFact :: SynTree BinOp Char -> Bool
isFact (Binary Impl (Leaf '1') (Leaf '1')) = False
isFact (Binary Impl (Leaf '1') (Leaf _)) = True
isFact _ = False

charFromFact :: SynTree BinOp Char -> Char
charFromFact (Binary Impl (Leaf '1') (Leaf a)) = a
charFromFact _ = error "Cannot get Char from not a fact."

getFacts :: [SynTree BinOp Char] -> [Char]
getFacts = map charFromFact . filter isFact

findSolution :: SynTree BinOp Char -> (Bool, Protocol)
findSolution formula = startAlg (markNext allClauses) (startProtocol facts)
  where
    facts = getFacts allClauses
    allClauses = getClauses formula

    startProtocol :: [Char] -> Protocol
    startProtocol cs = (1, map (1,) cs)

    addStep :: Char -> Protocol -> Protocol
    addStep c (step, record) = (step+1, record ++ [(step+1,c)])

    startAlg :: Maybe [SynTree BinOp Char] -> Protocol -> (Bool, Protocol)
    startAlg Nothing protocol = (False, protocol)
    startAlg (Just []) protocol = (True, protocol)
    startAlg (Just cs) protocol =
      let updatedProtocol = case toBeMarked cs of Nothing -> protocol
                                                  Just '0' -> protocol
                                                  Just c  -> addStep c protocol
      in startAlg (markNext cs) updatedProtocol

toBeMarked :: [SynTree BinOp Char] -> Maybe Char
toBeMarked clauses = case getFacts clauses of
  []    -> Nothing
  (x:_) -> Just x

markNext :: [SynTree BinOp Char] -> Maybe [SynTree BinOp Char]
markNext clauses = maybe (Just []) process (toBeMarked clauses)
  where
    process '0' = Nothing
    process a   = Just $ map (delConj . replace a) clauses

    replace x = fmap (\a -> if a == x then '1' else a)

    delConj :: SynTree BinOp Char -> SynTree BinOp Char
    delConj (Binary Impl (Binary And a b) c)
      | onlyOnes a = Binary Impl b c
      | onlyOnes b = Binary Impl a c
    delConj tree = tree
    onlyOnes = all (=='1') . collectLeaves
