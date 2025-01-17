{-# language QuasiQuotes #-}
{-# language OverloadedStrings #-}

module FindLiarTask (taskData,
                     makeHintsAndFormula) where

import Data.Text (Text)
import Data.String.Interpolate (i)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen
import Trees.Types (BinOp(..), SynTree(..))
import BuildHints (hintFromFormula, isNot)


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
    xYnOrNotYn = if xw == yw        then Leaf yn       else Not (Leaf yn)
    yZnOrNotZn = if yw == zw        then Leaf zn       else Not (Leaf zn)
    zXnOrNotXn = if v               then Leaf xn       else Not (Leaf xn)
    zYnOrNotYn = if (xw == yw) == v then Not (Leaf yn) else Leaf yn

    parts = [px, py, pz]
    px = Binary Equi (Leaf xn) xYnOrNotYn
    py = Binary Equi (Leaf yn) yZnOrNotZn
    pz = Binary Equi (Leaf zn) $ (if xn > yn then flip else id) (Binary (if zw then Or else And)) zXnOrNotXn zYnOrNotYn

    hints = [hx, hy, hz]
    hx = [i|#{xn} sagt: "#{yn}#{if isNot xYnOrNotYn then " lügt" else " sagt die Wahrheit"::String}."|]
    hy = [i|#{yn} sagt: "#{zn}#{if isNot yZnOrNotZn then " lügt" else " sagt die Wahrheit"::String}."|]
    hz = hintFromFormula pz
