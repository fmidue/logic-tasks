module Tasks.SuperfluousBrackets.PrintSuperfluousBrackets(
    superfluousBracketsDisplay,
    sameAssociativeOperatorAdjacentSerial,
)where

import Test.QuickCheck (Gen, frequency, elements, choose)

import Trees.Types (SynTree (..), BinOp(..), showOperator, showOperatorNot)
import Trees.Helpers (treeNodes, numberAllBinaryNodes)

superfluousBracketsDisplay :: SynTree BinOp Char -> Integer -> Gen String
superfluousBracketsDisplay synTree brackets =
    let synTreeWithSerial = numberAllBinaryNodes synTree
        serialsOfSameOperator = sameAssociativeOperatorAdjacentSerial synTreeWithSerial Nothing
    in  do
        serial <- elements serialsOfSameOperator
        rootDisplay synTreeWithSerial (brackets -1) serial

rootDisplay :: SynTree (BinOp, Integer) Char -> Integer -> Integer -> Gen String
rootDisplay (Leaf _) _ _ =  error "can not have only one node"
rootDisplay synTree@(Not a) brackets serial = do
    ifUseBrackets <- frequency
      [ (fromIntegral brackets, return True)
      , (fromIntegral (treeNodes synTree - brackets)
      , return False)
      ]
    if ifUseBrackets
    then do
        formula <- nonRootDisplay a (brackets - 1) Nothing (Just serial)
        return ("(" ++ showOperatorNot ++ formula ++ ")")
    else do
        formula <- nonRootDisplay a brackets Nothing (Just serial)
        return (showOperatorNot ++ formula)
rootDisplay (Binary operWithSerial a b) brackets serial =
    allocateBracketToSubtree a b operWithSerial brackets False Nothing (Just serial)

allocateBracketToSubtree
    :: SynTree (BinOp, Integer) Char
    -> SynTree (BinOp, Integer) Char
    -> (BinOp, Integer)
    -> Integer
    -> Bool
    -> Maybe BinOp
    -> Maybe Integer
    -> Gen String
allocateBracketToSubtree a b (oper, nowSerial) brackets hasFather fatherOperator serial
    | Just nowSerial == serial = do
        formula <- allocateBracketToSubtree
          a
          b
          (oper, error "never gonna need this")
          brackets
          hasFather
          fatherOperator
          Nothing
        return ("(" ++ formula ++ ")")
    | otherwise = let rightNodes =  treeNodes b
                      leftNodes =  treeNodes a
                in  do
                ifUseBrackets <- frequency
                  [ (fromIntegral brackets, return True)
                  , (fromIntegral (rightNodes + leftNodes + 1 - brackets), return False)
                  ]
                let brackets' = if ifUseBrackets then brackets - 1 else brackets
                    addBracket =
                      (if ifUseBrackets then 1 else 0) +
                      (if not hasFather || (fatherOperator == Just oper && (oper == And || oper == Or)) then 0 else 1)
                leftBrackets <- choose (max 0 (brackets' - rightNodes) , min leftNodes brackets')
                leftFormula <- nonRootDisplay a leftBrackets (Just oper) serial
                rightFormula <- nonRootDisplay b (brackets' - leftBrackets) (Just oper) serial
                return $
                  replicate addBracket '(' ++
                  leftFormula ++ " " ++
                  showOperator oper ++ " " ++
                  rightFormula ++
                  replicate addBracket ')'

nonRootDisplay :: SynTree (BinOp, Integer) Char -> Integer -> Maybe BinOp -> Maybe Integer -> Gen String
nonRootDisplay (Leaf a) 0 _ _ = return [a]
nonRootDisplay (Leaf a) 1 _ _ = return ("("++ (a : ")"))
nonRootDisplay synTree@(Not a) brackets _ serial = do
    ifUseBrackets <- frequency
      [ (fromIntegral brackets, return True)
      , (fromIntegral (treeNodes synTree - brackets)
      , return False)
      ]
    if ifUseBrackets
    then do
        formula <- nonRootDisplay a (brackets - 1) Nothing serial
        return ("(" ++ showOperatorNot ++ formula ++ ")")
    else do
        formula <- nonRootDisplay a brackets Nothing serial
        return (showOperatorNot ++ formula)
nonRootDisplay (Binary operWithSerial a b) brackets fatherOperator serial =
    allocateBracketToSubtree a b operWithSerial brackets True fatherOperator serial
nonRootDisplay (Leaf _) _ _ _ = error "All relevant cases handled!"

sameAssociativeOperatorAdjacentSerial :: SynTree (BinOp, Integer) c -> Maybe BinOp -> [Integer]
sameAssociativeOperatorAdjacentSerial (Leaf _) _ = []
sameAssociativeOperatorAdjacentSerial (Not a) _ = sameAssociativeOperatorAdjacentSerial a Nothing
sameAssociativeOperatorAdjacentSerial (Binary (oper, serial) a b) fatherOper
    | Just oper == fatherOper && (oper == And || oper == Or) =
        serial : sameAssociativeOperatorAdjacentSerial a (Just oper) ++
        sameAssociativeOperatorAdjacentSerial b (Just oper)
    | otherwise =
        sameAssociativeOperatorAdjacentSerial a (Just oper) ++ sameAssociativeOperatorAdjacentSerial b (Just oper)
