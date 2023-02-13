module Formula.Resolution
       (
         genRes
       , resolve
       , resolvable
       , resolvableWith
       , applySteps
       ) where


import qualified Data.Set as Set
import qualified SAT.MiniSat as Sat

import Data.Set (empty,Set)
import Data.Maybe (isJust)
import Test.QuickCheck (Gen,choose,elements,shuffle)

import Formula.Types
import Formula.Util



resolve :: Clause -> Clause -> Literal -> Maybe Clause
resolve (Clause x) (Clause y) literal
    | literal `Set.member` x = if opposite literal `Set.member` y
        then Just (Clause withoutLit)
        else Nothing

    | literal `Set.member` y = resolve (Clause y) (Clause x) literal
    | otherwise = Nothing
  where
    withoutLit = Set.union x y Set.\\ Set.fromList [literal,opposite literal]



resolvable :: Clause -> Clause -> Bool
resolvable c1 c2
    | isEmptyClause c1 || isEmptyClause c2 = False
    | otherwise = any (isJust . resolve c1 c2) lits
  where
    lits = literals c1





resolvableWith :: Clause -> Clause -> Maybe Literal
resolvableWith c1 c2
    | isEmptyClause c1 || isEmptyClause c2 = Nothing
    | length possible == 1 = Just (head possible)
    | otherwise = Nothing
  where
    lits = atomics c1
    possible = filter (isJust . resolve c1 c2) lits






applyStep :: [Clause] -> (Clause,Clause,Clause) -> Maybe [Clause]
applyStep [] _ = Just []
applyStep xs (c1,c2,resol)
    | c1 `notElem` xs || c2 `notElem` xs = Nothing
    | otherwise = do
        literal <- resolvableWith c1 c2
        new <- resolve c1 c2 literal
        if new == resol
          then pure (new:xs)
          else Nothing




applySteps :: [Clause] -> [(Clause,Clause,Clause)] -> Maybe [Clause]
applySteps [] _ = Just []
applySteps xs [] = Just xs
applySteps xs (y:ys) = applyStep xs y >>= flip applySteps ys




genRes :: (Int,Int) -> Int -> [Char] -> Gen [Clause]
genRes (minLen,maxLen) steps lits = do
    clauses <- buildClauses lits empty 0
    shuffled <- shuffle (Set.toList clauses)
    pure (map Clause shuffled)
  where
    buildClauses :: [Char] -> Set (Set Literal) -> Int -> Gen (Set (Set Literal))
    buildClauses xs ys runs
        | runs >= 100 = buildClauses xs empty 0
        | Set.size ys >= steps+1  = pure ys
        | otherwise =
            if Set.null ys
              then do
                chosenChar <- elements xs
                let
                  startSet = Set.fromList [Set.singleton (Literal chosenChar),Set.singleton (Not chosenChar)]
                buildClauses xs startSet 0
              else do
                let
                  underMin = Set.filter (\clause -> Set.size clause < minLen) ys
                  underMax = Set.filter (\clause -> Set.size clause <= maxLen) ys
                chosenClause <- setElements (if Set.null underMin then underMax else underMin)
                let
                  chooseableLits = filter (\lit ->
                    Literal lit `Set.notMember` chosenClause && Not lit `Set.notMember` chosenClause) xs
                if null chooseableLits
                    then buildClauses xs ys (runs+1)
                    else do
                      let clauseSize = Set.size chosenClause
                      choice <- if clauseSize == 1 || chosenClause `Set.member` underMin
                            then return (1 :: Int)
                            else
                              if clauseSize == maxLen
                                then return 2
                                else choose (1,2)
                      chosenChar <- elements chooseableLits
                      if choice == 1
                        then checkValidAndInsert (Literal chosenChar) chosenClause clauseSize 0
                        else do
                          firstAmount <- choose (1, clauseSize-1)
                          chosenSign <- elements [Literal chosenChar, Not chosenChar]
                          checkValidAndInsert chosenSign chosenClause firstAmount firstAmount
      where
        checkValidAndInsert :: Literal -> Set Literal -> Int -> Int -> Gen (Set (Set Literal))
        checkValidAndInsert lit clause get leave = do
            shuffledClause <- shuffle (Set.toList clause)
            let
              newClause1 = Set.fromList (lit : take get shuffledClause)
              newClause2 = Set.fromList (opposite lit : drop leave shuffledClause)
              newSet = Set.insert newClause2 (Set.insert newClause1 (Set.delete clause ys))
              subSets = Set.delete (Set.map Clause newSet) $ Set.powerSet (Set.map Clause newSet)
              listForm = map Set.toList (Set.toList subSets)
              satForm = map ((Sat.satisfiable . Sat.All) . map convert) listForm
              (toInsert,newRuns) = if and satForm then (newSet,0) else (ys,runs+1)
            buildClauses xs toInsert newRuns



setElements :: Set a -> Gen a
setElements set
    | null set = error "setElements used with empty set."
    | otherwise = (`Set.elemAt` set) `fmap` choose (0, Set.size set - 1)
