module Trees.Generate (
 genSynTree,
 syntaxShape,
) where

import Test.QuickCheck (choose, Gen, oneof, shuffle, suchThat, elements)
import Test.QuickCheck.Gen (vectorOf)

import Trees.Types (SynTree(..), BinOp(..), allBinaryOperators)
import Trees.Helpers (
  collectLeaves,
  relabelShape,
  maxNodesForDepth,
  consecutiveNegations,
  numOfUniqueBinOpsInSynTree,
  treeDepth)

chooseList :: Bool -> [BinOp]
chooseList allowArrowOperators = if allowArrowOperators
        then allBinaryOperators
        else [And, Or]

randomList :: [c] -> [c] -> Integer -> Gen [c]
randomList availableLetters atLeastOccurring listLength = let
    restLength = fromIntegral listLength - length atLeastOccurring
    in do
        randomRest <- vectorOf restLength (elements availableLetters)
        shuffle (atLeastOccurring ++ randomRest)

genSynTree ::
  (Integer, Integer)
  -> Integer
  -> Integer
  -> [c]
  -> Integer
  -> Bool
  -> Integer
  -> Integer
  -> Gen (SynTree BinOp c)
genSynTree
  (minNodes, maxNodes)
  minDepth
  maxDepth
  availableLetters
  atLeastOccurring
  allowArrowOperators
  maxConsecutiveNegations
  minUniqueBinOps = do
    sample <-
      (do nodes <- choose (minNodes, maxNodes) `suchThat` if hasNegations then const True else odd
          syntaxShape nodes maxDepth allowArrowOperators hasNegations
            `suchThat` \synTree ->
              checkAtLeastOccurring synTree &&
              checkMinUniqueOps synTree &&
              (not hasNegations || (consecutiveNegations synTree <= maxConsecutiveNegations))
        ) `suchThat` \synTree -> treeDepth synTree >= minDepth
    usedList <- randomList availableLetters (take (fromIntegral atLeastOccurring) availableLetters) $
           fromIntegral $ length $ collectLeaves sample
    return (relabelShape sample usedList)
  where hasNegations = maxConsecutiveNegations /= 0
        checkAtLeastOccurring synTree = fromIntegral (length (collectLeaves synTree)) >= atLeastOccurring
        checkMinUniqueOps synTree = numOfUniqueBinOpsInSynTree synTree >= minUniqueBinOps

syntaxShape :: Integer -> Integer -> Bool -> Bool -> Gen (SynTree BinOp ())
syntaxShape nodes maxDepth allowArrowOperators allowNegation
    | nodes == 1 = positiveLiteral
    | nodes == 2 = negativeLiteral
    | not allowNegation = oneof mapBinaryOperator
    | maxNodesForDepth (maxDepth - 1) < nodes - 1 = oneof mapBinaryOperator
    | otherwise = oneof $ negativeForm : mapBinaryOperator
    where
        mapBinaryOperator = map (binaryOperator nodes maxDepth allowArrowOperators allowNegation . Binary) $
          chooseList allowArrowOperators
        negativeForm = negativeFormula nodes maxDepth allowArrowOperators

binaryOperator
    :: Integer
    -> Integer
    -> Bool -> Bool
    -> (SynTree BinOp ()
    -> SynTree BinOp ()
    -> SynTree BinOp ())
    -> Gen (SynTree BinOp ())
binaryOperator nodes maxDepth allowArrowOperators allowNegation operator =
    let minNodesPerSide = max 1 (restNodes - maxNodesForDepth newMaxDepth)
        restNodes = nodes - 1
        newMaxDepth = maxDepth - 1
    in  do
        leftNodes <- choose (minNodesPerSide , restNodes - minNodesPerSide)
          `suchThat` \leftNodes -> allowNegation || odd leftNodes
        leftTree <- syntaxShape leftNodes newMaxDepth allowArrowOperators allowNegation
        rightTree <- syntaxShape (restNodes - leftNodes ) newMaxDepth allowArrowOperators allowNegation
        return (operator leftTree rightTree)

negativeFormula :: Integer -> Integer -> Bool -> Gen (SynTree BinOp ())
negativeFormula nodes maxDepth allowArrowOperators =
    let restNodes = nodes - 1
        newMaxDepth = maxDepth - 1
    in  do
        e <- syntaxShape restNodes newMaxDepth allowArrowOperators True
        return (Not e)

negativeLiteral ::  Gen (SynTree o ())
negativeLiteral = Not <$> positiveLiteral

positiveLiteral :: Gen (SynTree o ())
positiveLiteral = return (Leaf ())
