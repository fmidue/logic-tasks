{-# language QuasiQuotes #-}
{-# language OverloadedStrings #-}

module Werluegt where

import Data.Text               (Text)
import Data.String.Interpolate (i)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen
import Trees.Types (PropFormula(..), BinOp(..))



taskData :: Gen ((Char, Bool), (Char, Bool), (Char, Bool), Bool)
taskData = do
  permutation <- shuffle ['A','B','C']
  values <- vectorOf 3 arbitrary
  v <- arbitrary
  return (zip permutation values !! 0, zip permutation values !! 1, zip permutation values !! 2, v)


makeHintsAndFormula :: ((Char, Bool), (Char, Bool), (Char, Bool), Bool) -> (PropFormula Char, [Text])
makeHintsAndFormula ((xn, xw), (yn, yw), (zn, zw), v) = (formula, hints)
  where
    negx y = if xw == yw then Atomic y else Neg (Atomic y)
    negy z = if yw == zw then Atomic z else Neg (Atomic z)
    op = if zw then Or else And
    vnegz y = if (xw == yw) == v then Neg (Atomic y) else Atomic y
    vneg x = if v then Atomic x else Neg (Atomic x)

    parts = [p1, p2, p3]
    p1 = Assoc Equi (Atomic xn) (negx yn)                                   -- X_n <=> ?Y_n
    p2 = Assoc Equi (Atomic yn) (negy zn)                                   -- Y_n <=> ?Z_n
    p3 = Assoc Equi (Atomic zn) (Brackets (Assoc op (vneg xn) (vnegz yn)))  -- Z_n <=> (?X_n op ?Y_n)
    formula = foldr1 (Assoc And) (map Brackets parts)

    hints = [h1, h2, h3]
    h1 = [i|#{xn} sagt: #{yn} #{if isNeg (negx yn) then lgt else sdw}.|]
    h2 = [i|#{yn} sagt: #{zn} #{if isNeg (negy zn) then lgt else sdw}.|]
    h3 = [i|#{zn} sagt: #{xn} #{if isNeg (vneg xn) == isNeg(vnegz yn) then undoderyn else aberoderyn}.|]

    isNeg :: PropFormula c -> Bool
    isNeg (Neg _) = True
    isNeg _ = False

    sdw :: String
    sdw = "sagt die Wahrheit"

    lgt :: String
    lgt = "lügt"

    whichOp :: BinOp -> String
    whichOp And = if isNeg (vneg xn) == isNeg (vnegz yn) then " und" else ", aber"
    whichOp Or = "oder"
    whichOp _ = "unbekannter Operator"

    undoderyn :: String
    undoderyn = [i|#{whichOp op} #{yn} #{if isNeg (vneg xn) then "lügen"::String else "sagen die Wahrheit"::String}|]

    aberoderyn :: String
    aberoderyn = [i|#{if isNeg (vneg xn) then lgt else sdw}#{whichOp op} #{yn} #{if isNeg (vnegz yn) then lgt else sdw}|]