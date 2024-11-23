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
    xYOrNotY = if xw == yw then Atomic yn else Neg (Atomic yn)
    yZOrNotZ = if yw == zw then Atomic zn else Neg (Atomic zn)
    zXOrNotX = if v then Atomic xn else Neg (Atomic xn)
    zYOrNotY = if (xw == yw) == v then Neg (Atomic yn) else Atomic yn
    zOperator :: String
      | zw                               = " oder"
      | isNeg zXOrNotX == isNeg zYOrNotY = " und"
      | otherwise                        = ", aber"

    parts = [p1, p2, p3]
    p1 = Assoc Equi (Atomic xn) xYOrNotY
    p2 = Assoc Equi (Atomic yn) yZOrNotZ
    p3 = Assoc Equi (Atomic zn) (Brackets (Assoc (if zw then Or else And) zXOrNotX zYOrNotY))
    -- formula = foldr1 (Assoc And) (map Brackets parts)

    hints = [h1, h2, h3]
    h1 = [i|#{xn} sagt: #{yn}#{if isNeg xYOrNotY then " lügt"::String else sdW}.|]
    h2 = [i|#{yn} sagt: #{zn}#{if isNeg yZOrNotZ then " lügt"::String else sdW}.|]
    h3 = [i|#{zn} sagt: #{xn}#{statement}.|]

    isNeg :: PropFormula c -> Bool
    isNeg (Neg _) = True
    isNeg _ = False

    sdW :: String
    sdW = " sagt die Wahrheit"

    statement :: String
    statement = if isNeg zXOrNotX == isNeg zYOrNotY
      then
        let truthstatement = if isNeg zXOrNotX
            then "lügen"::String
            else "sagen die Wahrheit"::String
        in [i|#{zOperator} #{yn} #{truthstatement}|]
      else
        let partX = if isNeg zXOrNotX then " lügt"::String else sdW
            partY = if isNeg zYOrNotY then " lügt"::String else sdW
        in [i|#{partX}#{zOperator} #{yn}#{partY}|]
