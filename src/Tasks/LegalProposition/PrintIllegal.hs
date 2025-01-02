{-# LANGUAGE TupleSections #-}

module Tasks.LegalProposition.PrintIllegal (
    illegalDisplay,
    ) where


import Test.QuickCheck (Gen, elements, frequency)

import Trees.Helpers (collectLeaves, treeNodes)
import Trees.Print (normalShow)
import Trees.Types (BinOp(..), SynTree(..), allBinaryOperators, showOperator, showOperatorNot)
import Data.List.Extra (replace)
import Tasks.LegalProposition.Config (PropErrorReason (..))
import Data.Bifunctor (Bifunctor(second, bimap))
import Data.Maybe (fromJust, catMaybes)




illegalDisplay :: SynTree BinOp Char -> Gen (String, PropErrorReason)
illegalDisplay (Leaf _) = (,IllegalOperand) <$> elements (showOperatorNot : map showOperator allBinaryOperators)
illegalDisplay synTree =
    let usedLiterals = collectLeaves synTree
    in bimap replace' fromJust <$> ifUseIllegal True False synTree usedLiterals
      where replace' = replace "_" "" . replace "_ " "" . replace " _" ""

ifUseIllegal :: Bool -> Bool -> SynTree BinOp Char -> String -> Gen (String, Maybe PropErrorReason)
ifUseIllegal useBug notFirstLayer synTree usedLiterals =
    let
      nodeNum = treeNodes synTree
    in
      if not useBug
        then return (normalShow synTree, Nothing)
        else second Just <$> frequency
          [ (1, implementIllegal notFirstLayer synTree usedLiterals)
          , (fromIntegral nodeNum - 1, subTreeIllegal notFirstLayer synTree usedLiterals)
          ]



subTreeIllegal ::Bool -> SynTree BinOp Char -> String -> Gen (String, PropErrorReason)
subTreeIllegal notFirstLayer (Binary operator a b) usedLiterals =
    allocateBugToSubtree notFirstLayer a b usedLiterals operator
subTreeIllegal _ (Not a) usedLiterals = do
    left <- ifUseIllegal True True a usedLiterals
    return $ bimap (showOperatorNot ++) fromJust left
subTreeIllegal _ (Leaf _) _ = error "This will not happen but must be write"



allocateBugToSubtree :: Bool -> SynTree BinOp Char -> SynTree BinOp Char -> String -> BinOp -> Gen (String, PropErrorReason)
allocateBugToSubtree notFirstLayer a b usedLiterals usedOperator = do
    ifUseBug <- elements [True, False]
    (left, er1) <- ifUseIllegal ifUseBug True a usedLiterals
    (right, er2) <- ifUseIllegal (not ifUseBug) True b usedLiterals
    let errorReason = head $ catMaybes [er1,er2]
    if notFirstLayer
    then return ("(" ++ left ++ " " ++ showOperator usedOperator ++ " " ++ right ++ ")", errorReason)
    else return (left ++ " " ++ showOperator usedOperator ++ " " ++ right, errorReason)



illegalShow :: Bool -> SynTree BinOp Char -> SynTree BinOp Char -> String -> BinOp -> Gen (String, PropErrorReason)
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



combineNormalShow :: SynTree BinOp Char -> SynTree BinOp Char -> String -> Bool -> Gen (String,PropErrorReason)
combineNormalShow a b "" False = return (normalShow a ++ " " ++ normalShow b, MissingOperator)
combineNormalShow a b "" True = return ("(" ++ normalShow a ++ " " ++ normalShow b ++ ")", MissingOperator)
combineNormalShow a b replacedOperator False = return (normalShow a ++ " " ++ replacedOperator ++ " " ++ normalShow b, IllegalOperator)
combineNormalShow a b replacedOperator True =
    return ("(" ++ normalShow a ++ " " ++ replacedOperator ++ " " ++ normalShow b ++ ")", IllegalOperator)



implementIllegal :: Bool -> SynTree BinOp Char -> String -> Gen (String, PropErrorReason)
implementIllegal notFirstLayer (Binary operator a b) usedLiterals =
    illegalShow notFirstLayer a b usedLiterals operator
implementIllegal _ (Not a) usedLiterals = do
    letter <- elements usedLiterals
    chosenOp <- elements ([letter] : map showOperator allBinaryOperators)
    return (chosenOp ++ (' ' : normalShow a), if chosenOp == [letter] then MissingOperator else MissingOperand)
implementIllegal _ (Leaf _) _ = do
    operator <- elements (showOperatorNot : map showOperator allBinaryOperators)
    chosenOp <- elements [operator,"_"]
    return (chosenOp, if chosenOp == "_" then MissingOperand else IllegalOperand)



illegalParentheses :: SynTree BinOp Char -> SynTree BinOp Char -> BinOp -> [(Int, Gen (String, PropErrorReason))]
illegalParentheses  a b usedOperator =
    [ (1, return (formulaStr ++ ")", IllegalParentheses))
    , (1, return ("(" ++ formulaStr, IllegalParentheses))
    ]
    where formulaStr = normalShow a ++ " " ++ showOperator usedOperator ++ " " ++ normalShow b
