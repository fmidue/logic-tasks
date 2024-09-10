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

import Formula.Types hiding (Dnf(..), Con(..))
import Formula.Util
import Formula.Printing ()



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
applyStep xs (c1,c2,resolvent)
    | c1 `notElem` xs || c2 `notElem` xs = Nothing
    | otherwise = do
        literal <- resolvableWith c1 c2
        new <- resolve c1 c2 literal
        if new == resolvent
          then pure (new:xs)
          else Nothing




applySteps :: [Clause] -> [(Clause,Clause,Clause)] -> Maybe [Clause]
applySteps [] _ = Just []
applySteps xs [] = Just xs
applySteps xs (y:ys) = applyStep xs y >>= flip applySteps ys




genRes :: (Int,Int) -> Int -> [Char] -> Gen ([Clause], [ResStep])
genRes (minLen,maxLen) steps lits = do
    (clauses,rSteps) <- buildClauses lits (empty, []) 0
    shuffled <- shuffle (Set.toList clauses)
    pure (map Clause shuffled, rSteps)
  where
    toClause :: Set Literal -> Clause
    toClause = mkClause . Set.toList
    buildClauses :: [Char] -> (Set (Set Literal),[ResStep]) -> Int -> Gen (Set (Set Literal), [ResStep])
    buildClauses xs (ys,rs) runs
        | runs >= 100 = buildClauses xs (empty,[]) 0
        | Set.size ys >= steps+1  = pure (ys,rs)
        | otherwise =
            if Set.null ys
              then do
                chosenChar <- elements xs
                let
                  pos = Set.singleton $ Literal chosenChar
                  neg = Set.singleton $ Not chosenChar
                  startSet = Set.fromList [pos,neg]
                buildClauses xs (startSet,[Res (Left (toClause pos),Left (toClause neg), (toClause empty,Nothing))]) 0
              else do
                let
                  underMin = Set.filter (\clause -> Set.size clause < minLen) ys
                  underMax = Set.filter (\clause -> Set.size clause <= maxLen) ys
                chosenClause <- setElements (if Set.null underMin then underMax else underMin)
                let
                  chooseableLits = filter (\lit ->
                    Literal lit `Set.notMember` chosenClause && Not lit `Set.notMember` chosenClause) xs
                if null chooseableLits
                    then buildClauses xs (ys,rs) (runs+1)
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
                        then checkValidAndInsert (Literal chosenChar) chosenClause rs clauseSize 0
                        else do
                          firstAmount <- choose (1, clauseSize-1)
                          chosenSign <- elements [Literal chosenChar, Not chosenChar]
                          checkValidAndInsert chosenSign chosenClause rs firstAmount firstAmount
      where
        checkValidAndInsert :: Literal -> Set Literal -> [ResStep] -> Int -> Int -> Gen (Set (Set Literal),[ResStep])
        checkValidAndInsert lit clause resSteps get leave = do
            shuffledClause <- shuffle (Set.toList clause)
            let
              newClause1 = Set.fromList (lit : take get shuffledClause)
              newClause2 = Set.fromList (opposite lit : drop leave shuffledClause)
              newSet = Set.insert newClause2 (Set.insert newClause1 (Set.delete clause ys))
              subSets = Set.delete (Set.map Clause newSet) $ Set.powerSet (Set.map Clause newSet)
              listForm = map Set.toList (Set.toList subSets)
              satForm = map ((Sat.satisfiable . Sat.All) . map convert) listForm
              resultClause = Set.union newClause1 newClause2 `Set.difference` Set.fromList [lit, opposite lit]
              newResSteps = Res (Left (toClause newClause1), Left (toClause newClause2), (toClause resultClause,Nothing)) : resSteps
              (toInsert,newRuns,newSteps) = if and satForm then (newSet,0, newResSteps) else (ys,runs+1, resSteps)
            buildClauses xs (toInsert,newSteps) newRuns



setElements :: Set a -> Gen a
setElements set
    | null set = error "setElements used with empty set."
    | otherwise = (`Set.elemAt` set) `fmap` choose (0, Set.size set - 1)
