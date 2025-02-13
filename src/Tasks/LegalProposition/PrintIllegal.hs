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
import Data.Maybe (fromJust)
import Control.Applicative ((<|>))




illegalDisplay :: SynTree BinOp Char -> Gen (String, PropErrorReason)
illegalDisplay (Leaf _) = (,IllegalOperand) <$> elements (showOperatorNot : map showOperator allBinaryOperators)
illegalDisplay synTree =
    let usedAtoms = collectLeaves synTree
    in bimap replace' fromJust <$> ifUseIllegal True False synTree usedAtoms
      where replace' = replace "_" "" . replace "_ " "" . replace " _" ""

ifUseIllegal :: Bool -> Bool -> SynTree BinOp Char -> String -> Gen (String, Maybe PropErrorReason)
ifUseIllegal useBug notFirstLayer synTree usedAtoms =
    let
      nodeNum = treeNodes synTree
    in
      if not useBug
        then return (normalShow synTree, Nothing)
        else second Just <$> frequency
          [ (1, implementIllegal notFirstLayer synTree usedAtoms)
          , (fromIntegral nodeNum - 1, subTreeIllegal notFirstLayer synTree usedAtoms)
          ]



subTreeIllegal ::Bool -> SynTree BinOp Char -> String -> Gen (String, PropErrorReason)
subTreeIllegal notFirstLayer (Binary operator a b) usedAtoms =
    allocateBugToSubtree notFirstLayer a b usedAtoms operator
subTreeIllegal _ (Not a) usedAtoms = do
    left <- ifUseIllegal True True a usedAtoms
    return $ bimap (showOperatorNot ++) fromJust left
subTreeIllegal _ (Leaf _) _ = error "This will not happen but must be write"



allocateBugToSubtree :: Bool -> SynTree BinOp Char -> SynTree BinOp Char -> String -> BinOp -> Gen (String, PropErrorReason)
allocateBugToSubtree notFirstLayer a b usedAtoms usedOperator = do
    ifUseBug <- elements [True, False]
    (left, er1) <- ifUseIllegal ifUseBug True a usedAtoms
    (right, er2) <- ifUseIllegal (not ifUseBug) True b usedAtoms
    let errorReason = fromJust (er1 <|> er2)
    if notFirstLayer
    then return ("(" ++ left ++ " " ++ showOperator usedOperator ++ " " ++ right ++ ")", errorReason)
    else return (left ++ " " ++ showOperator usedOperator ++ " " ++ right, errorReason)



illegalShow :: Bool -> SynTree BinOp Char -> SynTree BinOp Char -> String -> BinOp -> Gen (String, PropErrorReason)
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



combineNormalShow :: SynTree BinOp Char -> SynTree BinOp Char -> String -> Bool -> Gen (String,PropErrorReason)
combineNormalShow a b "" False = return (normalShow a ++ " " ++ normalShow b, MissingOperator)
combineNormalShow a b "" True = return ("(" ++ normalShow a ++ " " ++ normalShow b ++ ")", MissingOperator)
combineNormalShow a b replacedOperator False = return (normalShow a ++ " " ++ replacedOperator ++ " " ++ normalShow b, IllegalOperator)
combineNormalShow a b replacedOperator True =
    return ("(" ++ normalShow a ++ " " ++ replacedOperator ++ " " ++ normalShow b ++ ")", IllegalOperator)



implementIllegal :: Bool -> SynTree BinOp Char -> String -> Gen (String, PropErrorReason)
implementIllegal notFirstLayer (Binary operator a b) usedAtoms =
    illegalShow notFirstLayer a b usedAtoms operator
implementIllegal _ (Not a) usedAtoms = do
    letter <- elements usedAtoms
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
