module Horn where

import Trees.Types (BinOp(..), SynTree(..))

type Protocol = (Int, [(Int, Char)])

makeHornformula :: SynTree BinOp Char
makeHornformula =
  case clauses of
    [] -> error "Cannot build formula witch no clauses."
    _  -> foldr1 (Binary And) clauses
  where
    clauses =
      [ Binary Impl (Leaf 'B') (Leaf 'A')
      , Binary Impl (Leaf '1') (Leaf 'B')
      , Binary Impl (Leaf 'B') (Leaf 'C')
      , Binary Impl (Leaf 'C') (Leaf '0')
      ]

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
findSolution formula = letsGo (markNext allClauses) p
  where
    p = startProtocol facts
    facts = getFacts allClauses
    allClauses = getClauses formula

    startProtocol :: [Char] -> Protocol
    startProtocol cs = (1, map (1,) cs)

    addStep :: Char -> Protocol -> Protocol
    addStep c (step, record) = (step+1, record ++ [(step+1,c)])

    letsGo :: Maybe [SynTree BinOp Char] -> Protocol -> (Bool, Protocol)
    letsGo Nothing protocol = (False, protocol)
    letsGo (Just []) protocol = (True, protocol)
    letsGo (Just cs) protocol =
      let updatedProtocol = case toBeMarked cs of Nothing -> protocol
                                                  Just '0' -> protocol
                                                  Just c  -> addStep c protocol
      in letsGo (markNext cs) updatedProtocol

toBeMarked :: [SynTree BinOp Char] -> Maybe Char
toBeMarked clauses = case getFacts clauses of
  []    -> Nothing
  (x:_) -> Just x

markNext :: [SynTree BinOp Char] -> Maybe [SynTree BinOp Char]
markNext clauses = maybe (Just []) process (toBeMarked clauses)
  where
    process '0' = Nothing
    process a   = Just $ map (mark a) clauses

    mark :: Char -> SynTree BinOp Char -> SynTree BinOp Char
    mark x (Binary Impl (Binary And a b) c)
      | a == Leaf '1' = Binary Impl b c
      | b == Leaf '1' = Binary Impl a c
      | otherwise = Binary Impl (Binary And (replace x a) (replace x b)) c
    mark x (Binary Impl (Leaf '1') b) = Binary Impl (Leaf '1') (replace x b)
    mark x (Binary Impl a b) = Binary Impl (replace x a) b
    mark _ _ = error "should not happen"

    replace z (Leaf y)
          | z == y = Leaf '1'
          | otherwise = Leaf y
    replace _ _= error "help"
