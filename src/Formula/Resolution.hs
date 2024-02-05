{-# LANGUAGE TupleSections #-}
module Formula.Resolution
       (
         genRes
       , resolve
       , resolvable
       , resolvableWith
       , applySteps
       , computeResSteps
       , showResSteps
       ) where


import qualified Data.Set as Set
import qualified SAT.MiniSat as Sat

import Data.Set (empty,Set)
import Data.Maybe (isJust, fromJust)
import Test.QuickCheck (Gen,choose,elements,shuffle)

import Formula.Types hiding (Dnf(..), Con(..))
import Formula.Util
import Data.List (find, elemIndex)
import Data.Containers.ListUtils (nubOrd)
import Text.PrettyPrint.Leijen.Text (Pretty(pretty))
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

----------------------------------------------------------------------------------------------------------

resolutions :: [([Clause], Clause)] -> [([Clause], Clause)]
resolutions [] = []
resolutions clauseMap@((cs, r):xs) = nubOrd $ (cs,r) : [ ([x,y], fromJust res) |
  x <- allClauses,
  l <- Set.toList (literalSet x),
  y <- allClauses,
  let res = resolve x y l, isJust res]  ++ resolutions xs
    where allClauses = map snd clauseMap

solution' :: [([Clause], Clause)] -> [([Clause], Clause)]
solution' xs = if any (\(_, Clause x) -> null x) xs then xs else solution' (resolutions xs)

reconstructSolution :: Clause -> [([Clause], Clause)] -> [([Clause], Clause)]
reconstructSolution c xs = if isJust rOrigin then [ fromJust rOrigin ]
                                             else recursiveLeft ++ recursiveRight ++ [(fst (fromJust r) ,c)]
  where rOrigin = find (\(ps,x) -> x == c && null ps) xs
        r = find (\(ps,x) -> x == c && not (null ps)) xs
        recursiveLeft = reconstructSolution (head (fst (fromJust r))) xs
        recursiveRight = reconstructSolution (last (fst (fromJust r))) xs

applyNum :: [Clause] -> [([Clause], Clause)] -> [([Clause], Clause, Int)]
applyNum _ [] = []
applyNum original xs = map (\(ys, c) -> (ys, c, fromJust (elemIndex c correctedOriginal) +1)) old ++ zipped
  where
    old = filter (\(ps, _) -> null ps) xs
    new = filter (\(ps, _) -> not (null ps)) xs
    correctedOriginal = Set.toList $ clauseSet $ mkCnf original
    zipped = zipWith (curry (\((ys, c),i) -> (ys, c,i))) new [length original + 1..]

convertSteps :: [([Clause], Clause, Int)] -> [ResStep]
convertSteps [] = []
convertSteps xs = map mapFn new
  where new = filter (\(a,_,_) -> not (null a)) xs
        mapFn (ys,c,i) = let (_,_,l) = fromJust (find (\(_,b,_) -> b == head ys) xs)
                             (_,_,r) = fromJust (find (\(_,b,_) -> b == last ys) xs)
                          in Res (Right l, Right r, (c, Just i))

removeNumberAtEmptyClause :: ResStep -> ResStep
removeNumberAtEmptyClause res@(Res (a,b,(Clause c,_)))
  | null c = Res (a,b,(Clause c,Nothing))
  | otherwise = res

showResSteps :: [ResStep] -> [String]
showResSteps [] = []
showResSteps [x] = [show $ pretty (removeNumberAtEmptyClause x)]
showResSteps (x:xs) = show (pretty (removeNumberAtEmptyClause x)) : showResSteps xs

computeResSteps :: [Clause] -> [ResStep]
computeResSteps clauses = convertSteps (applyNum clauses reconstructed)
  where reconstructed = reconstructSolution (Clause empty) (solution' (map ([],) clauses))
