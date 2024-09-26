{-# LANGUAGE RecordWildCards #-}
module Trees.Generate (
 genSynTree,
 syntaxShape,
) where

import Test.QuickCheck (choose, Gen, shuffle, suchThat, elements, frequency)
import Test.QuickCheck.Gen (vectorOf)

import Trees.Types (SynTree(..), BinOp(..))
import Trees.Helpers (
  collectLeaves,
  relabelShape,
  maxNodesForDepth,
  consecutiveNegations,
  numOfUniqueBinOpsInSynTree,
  treeDepth)
import Tasks.SynTree.Config (SynTreeConfig(..))
import Data.Bifunctor (Bifunctor(second))
import Data.Map (Map)
import qualified Data.Map as Map (toList)
import Data.Tuple (swap)


randomList :: [c] -> [c] -> Integer -> Gen [c]
randomList availableLetters atLeastOccurring listLength = let
    restLength = fromIntegral listLength - length atLeastOccurring
    in do
        randomRest <- vectorOf restLength (elements availableLetters)
        shuffle (atLeastOccurring ++ randomRest)

genSynTree :: SynTreeConfig -> Gen (SynTree BinOp Char)
genSynTree SynTreeConfig{..} = do
    sample <-
      (do nodes <- choose (minNodes, maxNodes) `suchThat` if hasNegations then const True else odd
          syntaxShape nodes maxDepth binOpFrequencies negOpFrequency hasNegations
            `suchThat` \synTree ->
              checkMinAmountOfUniqueAtoms synTree &&
              checkMinUniqueOps synTree &&
              (not hasNegations || (consecutiveNegations synTree <= maxConsecutiveNegations))
        ) `suchThat` \synTree -> treeDepth synTree >= minDepth
    usedList <- randomList availableAtoms (take (fromIntegral minAmountOfUniqueAtoms) availableAtoms) $
           fromIntegral $ length $ collectLeaves sample
    return (relabelShape sample usedList)
  where hasNegations = maxConsecutiveNegations /= 0
        checkMinAmountOfUniqueAtoms synTree =
          fromIntegral (length (collectLeaves synTree)) >= minAmountOfUniqueAtoms
        checkMinUniqueOps synTree = numOfUniqueBinOpsInSynTree synTree >= minUniqueBinOperators

syntaxShape :: Integer -> Integer -> Map BinOp Int -> Int -> Bool -> Gen (SynTree BinOp ())
syntaxShape nodes maxDepth binOpFrequencies negOpFrequency allowNegation
    | nodes == 1 = positiveLiteral
    | nodes == 2 = negativeLiteral
    | not allowNegation = frequency mapBinaryOperator
    | maxNodesForDepth (maxDepth - 1) < nodes - 1 = frequency mapBinaryOperator
    | otherwise = frequency $ (negOpFrequency,negativeForm) : mapBinaryOperator
    where
        binOpFrequencies' = map swap $ Map.toList binOpFrequencies
        toGen = binaryOperator nodes maxDepth binOpFrequencies negOpFrequency allowNegation . Binary
        mapBinaryOperator = map (second toGen) binOpFrequencies'
        negativeForm = negativeFormula nodes maxDepth binOpFrequencies negOpFrequency

binaryOperator
    :: Integer
    -> Integer
    -> Map BinOp Int
    -> Int
    -> Bool
    -> (SynTree BinOp ()
    -> SynTree BinOp ()
    -> SynTree BinOp ())
    -> Gen (SynTree BinOp ())
binaryOperator nodes maxDepth binOpFrequencies negOpFrequency allowNegation operator =
    let minNodesPerSide = max 1 (restNodes - maxNodesForDepth newMaxDepth)
        restNodes = nodes - 1
        newMaxDepth = maxDepth - 1
    in  do
        leftNodes <- choose (minNodesPerSide , restNodes - minNodesPerSide)
          `suchThat` \leftNodes -> allowNegation || odd leftNodes
        leftTree <- syntaxShape leftNodes newMaxDepth binOpFrequencies negOpFrequency allowNegation
        rightTree <- syntaxShape (restNodes - leftNodes ) newMaxDepth binOpFrequencies negOpFrequency allowNegation
        return (operator leftTree rightTree)

negativeFormula :: Integer -> Integer ->  Map BinOp Int -> Int -> Gen (SynTree BinOp ())
negativeFormula nodes maxDepth binOpFrequencies negOpFrequency =
    let restNodes = nodes - 1
        newMaxDepth = maxDepth - 1
    in  do
        e <- syntaxShape restNodes newMaxDepth binOpFrequencies negOpFrequency True
        return (Not e)

negativeLiteral ::  Gen (SynTree o ())
negativeLiteral = Not <$> positiveLiteral

positiveLiteral :: Gen (SynTree o ())
positiveLiteral = return (Leaf ())
