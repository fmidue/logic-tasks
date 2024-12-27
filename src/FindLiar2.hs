module FindLiar2 where

import Data.Text (Text)
import Trees.Types (BinOp(..), SynTree(..))
import BuildHints (hintFromFormula)

makeHintsAndFormula :: ((Char, Bool), (Char, Bool), (Char, Bool)) -> ([SynTree BinOp Char], [Text])
makeHintsAndFormula ((xn, xw), (yn, yw), (zn, zw)) = (parts, hints)
  where
    xOperator  = if xw       then Or            else And
    xYnOrNotYn = if xw == yw then Leaf yn       else Not (Leaf yn)
    xZnOrNotZn = if xw == zw then Not (Leaf zn) else Leaf zn

    yOperator  = if yw       then Or            else And
    yXnOrNotXn = if yw == xw then Leaf xn       else Not (Leaf xn)
    yZnOrNotZn = if yw == zw then Leaf zn       else Not (Leaf zn)

    zOperator  = if zw       then And           else Or
    zXnOrNotXn = if zw == xw then Leaf xn       else Not (Leaf xn)
    zYnOrNotYn = if zw == yw then Leaf yn       else Not (Leaf yn)

    parts = [px, py, pz]
    px = Binary Equi (Leaf xn) (Binary xOperator xYnOrNotYn xZnOrNotZn)
    py = Binary Equi (Leaf yn) (Binary yOperator yXnOrNotXn yZnOrNotZn)
    pz = Binary Equi (Leaf zn) (Binary zOperator zXnOrNotXn zYnOrNotYn)

    hints = [hintFromFormula px, hintFromFormula py, hintFromFormula pz]
