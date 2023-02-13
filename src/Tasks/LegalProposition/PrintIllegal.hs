module Tasks.LegalProposition.PrintIllegal (
    illegalDisplay,
) where

import Test.QuickCheck (Gen, frequency, elements)

import Trees.Types (SynTree(..), BinOp(..), showOperator, showOperatorNot, allBinaryOperators)
import Trees.Helpers (treeNodes, collectLeaves)
import Trees.Print (normalShow)

illegalDisplay :: SynTree BinOp Char -> Gen String
illegalDisplay (Leaf _) = elements (showOperatorNot : map showOperator allBinaryOperators)
illegalDisplay synTree =
    let usedLiterals = collectLeaves synTree
    in ifUseIllegal True False synTree usedLiterals

ifUseIllegal :: Bool -> Bool -> SynTree BinOp Char -> String -> Gen String
ifUseIllegal useBug notFirstLayer synTree usedLiterals =
    let
      nodeNum = treeNodes synTree
    in
      if not useBug
        then return (normalShow synTree)
        else frequency
          [ (1, implementIllegal notFirstLayer synTree usedLiterals)
          , (fromIntegral nodeNum - 1, subTreeIllegal notFirstLayer synTree usedLiterals)
          ]

subTreeIllegal ::Bool -> SynTree BinOp Char -> String -> Gen String
subTreeIllegal notFirstLayer (Binary oper a b) usedLiterals =
    allocateBugToSubtree notFirstLayer a b usedLiterals (showOperator oper)
subTreeIllegal _ (Not a) usedLiterals = do
    left <- ifUseIllegal True True a usedLiterals
    return (showOperatorNot ++ left)
subTreeIllegal _ (Leaf _) _ = error "This will not happen but must be write"

allocateBugToSubtree :: Bool -> SynTree BinOp Char -> SynTree BinOp Char -> String -> String -> Gen String
allocateBugToSubtree notFirstLayer a b usedLiterals usedOperator = do
    ifUseBug <- elements [True, False]
    left <- ifUseIllegal ifUseBug True a usedLiterals
    right <- ifUseIllegal (not ifUseBug) True b usedLiterals
    if notFirstLayer
    then return ("(" ++ left ++ " " ++ usedOperator ++ " " ++ right ++ ")")
    else return (left ++ " " ++ usedOperator ++ " " ++ right)

illegalShow :: Bool -> SynTree BinOp Char -> SynTree BinOp Char -> String -> String -> Gen String
illegalShow notFirstLayer a b usedLiterals usedOperator =
    if notFirstLayer
    then  do
        letter <- elements usedLiterals
        frequency (map (\(probability, replacedOperator) ->
          (probability, combineNormalShow a b replacedOperator True))
            [(2, ""), (2, showOperatorNot), (2, [letter])] ++ illegalParentheses a b usedOperator)
    else  do
        letter <- elements usedLiterals
        frequency (map (\(probability, replacedOperator) ->
          (probability, combineNormalShow a b replacedOperator False)) [(2, ""), (1, showOperatorNot), (1, [letter])])

combineNormalShow :: SynTree BinOp Char -> SynTree BinOp Char -> String -> Bool -> Gen String
combineNormalShow a b replacedOperator False = return (normalShow a ++ " " ++ replacedOperator ++ " " ++ normalShow b)
combineNormalShow a b replacedOperator True =
    return $ "(" ++ normalShow a ++ " " ++ replacedOperator ++ " " ++ normalShow b ++ ")"


implementIllegal :: Bool -> SynTree BinOp Char -> String -> Gen String
implementIllegal notFirstLayer (Binary oper a b) usedLiterals =
    illegalShow notFirstLayer a b usedLiterals (showOperator oper)
implementIllegal _ (Not a) usedLiterals = do
    letter <- elements usedLiterals
    elements  $ map (++ (' ' : normalShow a)) ([letter] : map showOperator allBinaryOperators)
implementIllegal _ (Leaf _) _ = do
    oper <- elements (showOperatorNot : map showOperator allBinaryOperators)
    elements [oper,""]

illegalParentheses :: SynTree BinOp Char -> SynTree BinOp Char -> String -> [(Int, Gen String)]
illegalParentheses  a b usedOperator = [(1, return (formulaStr ++ ")")),(1, return ("(" ++ formulaStr))]
    where formulaStr = normalShow a ++ " " ++ usedOperator ++ " " ++ normalShow b
