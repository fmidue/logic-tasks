{-# language QuasiQuotes #-}
{-# language OverloadedStrings #-}

module FindLiarTask where

import Data.Text (Text)
import Data.String.Interpolate (i)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen
import Trees.Types (BinOp(..), SynTree(..))


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

makeHintsAndFormula :: ((Char, Bool), (Char, Bool), (Char, Bool), Bool) -> ([SynTree BinOp Char], [Text])
makeHintsAndFormula ((xn, xw), (yn, yw), (zn, zw), v) = (parts, hints)
  where
    xYnOrNotYn = if xw == yw then Leaf yn else Not (Leaf yn)
    yZnOrNotZn = if yw == zw then Leaf zn else Not (Leaf zn)
    zXnOrNotXn = if v then Leaf xn else Not (Leaf xn)
    zYnOrNotYn = if (xw == yw) == v then Not (Leaf yn) else Leaf yn
    zOperator :: String
      | zw                                   = " oder"
      | isNot zXnOrNotXn == isNot zYnOrNotYn = " und"
      | otherwise                            = ", aber"

    parts = [px, py, pz]
    px = Binary Equi (Leaf xn) xYnOrNotYn
    py = Binary Equi (Leaf yn) yZnOrNotZn
    pz = Binary Equi (Leaf zn) (Binary (if zw then Or else And) zXnOrNotXn zYnOrNotYn)

    hints = [hx, hy, hz]
    hx = [i|#{xn} sagt: "#{yn}#{if isNot xYnOrNotYn then " lügt" else sdW}."|]
    hy = [i|#{yn} sagt: "#{zn}#{if isNot yZnOrNotZn then " lügt" else sdW}."|]
    hz = [i|#{zn} sagt: "#{xn}#{statement}."|]

    isNot :: SynTree b c -> Bool
    isNot (Not _) = True
    isNot _       = False

    sdW :: String
    sdW = " sagt die Wahrheit"

    statement :: String
    statement = if isNot zXnOrNotXn == isNot zYnOrNotYn
      then
        let truthStatement :: String = if isNot zXnOrNotXn
            then "lügen"
            else "sagen die Wahrheit"
        in [i|#{zOperator} #{yn} #{truthStatement}|]
      else
        let partX = if isNot zXnOrNotXn then " lügt" else sdW
            partY = if isNot zYnOrNotYn then " lügt" else sdW
        in [i|#{partX}#{zOperator} #{yn}#{partY}|]
