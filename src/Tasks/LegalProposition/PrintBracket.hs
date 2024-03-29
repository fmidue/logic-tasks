
module Tasks.LegalProposition.PrintBracket (
    bracketDisplay
    ) where


import Test.QuickCheck (Gen, elements, frequency)

import Trees.Types (BinOp(..), SynTree(..), showOperator, showOperatorNot)
import Trees.Print (normalShow)




bracketDisplay :: SynTree BinOp Char -> Gen String
bracketDisplay (Binary operator a b) = allocateBracketToSubtree False a b (showOperator operator)
bracketDisplay (Leaf a)=  return ("("++ (a : ")"))
bracketDisplay (Not a) = do
    aFormula <- ifUseBracket True a
    return (showOperatorNot ++ aFormula)



ifUseBracket :: Bool -> SynTree BinOp Char -> Gen String
ifUseBracket useBracket (Leaf a) = if not useBracket then return [a] else return ['(', a, ')']
ifUseBracket useBracket synTree@(Not a) = let addPositions = notAndLeaves a in
    if not useBracket
    then return (normalShow synTree)
    else frequency [(1, return("(" ++ showOperatorNot ++ normalShow a ++ ")")), (addPositions, subTreeBracket synTree)]
ifUseBracket useBracket synTree@Binary {} =
    if not useBracket
    then return (normalShow synTree)
    else subTreeBracket synTree



subTreeBracket :: SynTree BinOp Char -> Gen String
subTreeBracket (Binary operator a b) = allocateBracketToSubtree True a b (showOperator operator)
subTreeBracket (Not a) = do
    left <- ifUseBracket True a
    return (showOperatorNot ++ left)
subTreeBracket (Leaf _) = error "This will never happen!"



allocateBracketToSubtree :: Bool -> SynTree BinOp Char -> SynTree BinOp Char -> String -> Gen String
allocateBracketToSubtree notFirstLayer a b usedOperator = do
    ifUseBug <- elements [True, False]
    left <- ifUseBracket ifUseBug a
    right <- ifUseBracket (not ifUseBug) b
    if notFirstLayer
    then return ("(" ++ left ++ " " ++ usedOperator ++ " " ++ right ++ ")")
    else return (left ++ " " ++ usedOperator ++ " " ++ right)



notAndLeaves :: SynTree o c -> Int
notAndLeaves (Binary _ a b) = notAndLeaves a + notAndLeaves b
notAndLeaves (Leaf _) = 1
notAndLeaves (Not a) = 1 + notAndLeaves a
