
module Tasks.LegalProposition.PrintIllegal (
    illegalDisplay,
    ) where


import Test.QuickCheck (Gen, elements, frequency)

import Trees.Helpers (collectLeaves, treeNodes)
import Trees.Print (normalShow)
import Trees.Types (BinOp(..), SynTree(..), allBinaryOperators, showOperator, showOperatorNot)
import Data.List.Extra (replace)




illegalDisplay :: SynTree BinOp Char -> Gen String
illegalDisplay (Leaf _) = elements (showOperatorNot : map showOperator allBinaryOperators)
illegalDisplay synTree =
    let usedAtoms = collectLeaves synTree
    in replace' <$> ifUseIllegal True False synTree usedAtoms
      where replace' = replace "_" "" . replace "_ " "" . replace " _" ""

ifUseIllegal :: Bool -> Bool -> SynTree BinOp Char -> String -> Gen String
ifUseIllegal useBug notFirstLayer synTree usedAtoms =
    let
      nodeNum = treeNodes synTree
    in
      if not useBug
        then return (normalShow synTree)
        else frequency
          [ (1, implementIllegal notFirstLayer synTree usedAtoms)
          , (fromIntegral nodeNum - 1, subTreeIllegal notFirstLayer synTree usedAtoms)
          ]



subTreeIllegal ::Bool -> SynTree BinOp Char -> String -> Gen String
subTreeIllegal notFirstLayer (Binary operator a b) usedAtoms =
    allocateBugToSubtree notFirstLayer a b usedAtoms operator
subTreeIllegal _ (Not a) usedAtoms = do
    left <- ifUseIllegal True True a usedAtoms
    return (showOperatorNot ++ left)
subTreeIllegal _ (Leaf _) _ = error "This will not happen but must be write"



allocateBugToSubtree :: Bool -> SynTree BinOp Char -> SynTree BinOp Char -> String -> BinOp -> Gen String
allocateBugToSubtree notFirstLayer a b usedAtoms usedOperator = do
    ifUseBug <- elements [True, False]
    left <- ifUseIllegal ifUseBug True a usedAtoms
    right <- ifUseIllegal (not ifUseBug) True b usedAtoms
    if notFirstLayer
    then return ("(" ++ left ++ " " ++ showOperator usedOperator ++ " " ++ right ++ ")")
    else return (left ++ " " ++ showOperator usedOperator ++ " " ++ right)



illegalShow :: Bool -> SynTree BinOp Char -> SynTree BinOp Char -> String -> BinOp -> Gen String
illegalShow notFirstLayer a b usedAtoms usedOperator =
    if notFirstLayer
    then  do
        letter <- elements usedAtoms
        frequency (map (\(probability, replacedOperator) ->
          (probability, combineNormalShow a b replacedOperator True))
            [(2, ""), (2, showOperatorNot), (2, [letter])] ++ illegalParentheses a b usedOperator)
    else  do
        letter <- elements usedAtoms
        frequency (map (\(probability, replacedOperator) ->
          (probability, combineNormalShow a b replacedOperator False)) [(2, ""), (1, showOperatorNot), (1, [letter])])



combineNormalShow :: SynTree BinOp Char -> SynTree BinOp Char -> String -> Bool -> Gen String
combineNormalShow a b "" False = return (normalShow a ++ " " ++ normalShow b)
combineNormalShow a b "" True = return ("(" ++ normalShow a ++ " " ++ normalShow b ++ ")")
combineNormalShow a b replacedOperator False = return (normalShow a ++ " " ++ replacedOperator ++ " " ++ normalShow b)
combineNormalShow a b replacedOperator True =
    return $ "(" ++ normalShow a ++ " " ++ replacedOperator ++ " " ++ normalShow b ++ ")"



implementIllegal :: Bool -> SynTree BinOp Char -> String -> Gen String
implementIllegal notFirstLayer (Binary operator a b) usedAtoms =
    illegalShow notFirstLayer a b usedAtoms operator
implementIllegal _ (Not a) usedAtoms = do
    letter <- elements usedAtoms
    elements  $ map (++ (' ' : normalShow a)) ([letter] : map showOperator allBinaryOperators)
implementIllegal _ (Leaf _) _ = do
    operator <- elements (showOperatorNot : map showOperator allBinaryOperators)
    elements [operator,"_"]



illegalParentheses :: SynTree BinOp Char -> SynTree BinOp Char -> BinOp -> [(Int, Gen String)]
illegalParentheses  a b usedOperator = [(1, return (formulaStr ++ ")")),(1, return ("(" ++ formulaStr))]
    where formulaStr = normalShow a ++ " " ++ showOperator usedOperator ++ " " ++ normalShow b
