{-# language QuasiQuotes #-}
{-# language OverloadedStrings #-}

module FindLiarTask where

import Data.Text (Text)
import Data.String.Interpolate (i)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen
import Trees.Types (PropFormula(..), BinOp(..))


taskData :: Gen ((Char, Bool), (Char, Bool), (Char, Bool), Bool)
taskData = do
  permutation <- shuffle ['A','B','C']
  values <- vectorOf 3 arbitrary
  v <- arbitrary
  case zip permutation values of
    [(p0,v0), (p1,v1), (p2,v2)]
      -> return ((p0,v0), (p1,v1), (p2,v2), v)
    _
      -> error "This is impossible!"

makeHintsAndFormula :: ((Char, Bool), (Char, Bool), (Char, Bool), Bool) -> ([PropFormula Char], [Text])
makeHintsAndFormula ((xn, xw), (yn, yw), (zn, zw), v) = (parts, hints)
  where
    xYnOrNotYn = if xw == yw then Atomic yn else Neg (Atomic yn)
    yZnOrNotZn = if yw == zw then Atomic zn else Neg (Atomic zn)
    zXnOrNotXn = if v then Atomic xn else Neg (Atomic xn)
    zYnOrNotYn = if (xw == yw) == v then Neg (Atomic yn) else Atomic yn
    zOperator :: String
      | zw                                   = " oder"
      | isNeg zXnOrNotXn == isNeg zYnOrNotYn = " und"
      | otherwise                            = ", aber"

    parts = [px, py, pz]
    px = Assoc Equi (Atomic xn) xYnOrNotYn
    py = Assoc Equi (Atomic yn) yZnOrNotZn
    pz = Assoc Equi (Atomic zn) (Brackets (Assoc (if zw then Or else And) zXnOrNotXn zYnOrNotYn))
    -- formula = foldr1 (Assoc And) (map Brackets parts)

    hints = [hx, hy, hz]
    hx = [i|#{xn} sagt: "#{yn}#{if isNeg xYnOrNotYn then " lügt"::String else sdW}."|]
    hy = [i|#{yn} sagt: "#{zn}#{if isNeg yZnOrNotZn then " lügt"::String else sdW}."|]
    hz = [i|#{zn} sagt: "#{xn}#{statement}."|]

    isNeg :: PropFormula c -> Bool
    isNeg (Neg _) = True
    isNeg _ = False

    sdW :: String
    sdW = " sagt die Wahrheit"

    statement :: String
    statement = if isNeg zXnOrNotXn == isNeg zYnOrNotYn
      then
        let truthStatement = if isNeg zXnOrNotXn
            then "lügen"::String
            else "sagen die Wahrheit"::String
        in [i|#{zOperator} #{yn} #{truthStatement}|]
      else
        let partX = if isNeg zXnOrNotXn then " lügt"::String else sdW
            partY = if isNeg zYnOrNotYn then " lügt"::String else sdW
        in [i|#{partX}#{zOperator} #{yn}#{partY}|]
