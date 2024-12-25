{-# language QuasiQuotes #-}
{-# language OverloadedStrings #-}

module FindLiar2 where

import Data.Text (Text)
import Data.String.Interpolate (i)
import Trees.Types (BinOp(..), SynTree(..))

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

hintFromFormula :: SynTree BinOp Char -> Text
hintFromFormula (Binary Equi (Leaf nameA) (Binary operator b c)) =
  [i|#{nameA} sagt: "#{nameFromLeaf b}#{word1}#{operatorName} #{nameFromLeaf c}#{word2}"|]
  where
    operatorName :: String
      | operator == Or                        = " oder"
      | operator == And && isNot b == isNot c = " und"
      | otherwise                             = ", aber"

    word1 :: String
      | isNot b == isNot c = ""
      | isNot b            = " lügt"
      | otherwise          = " sagt die Wahrheit"

    word2 :: String
      | isNot b && isNot c = " lügen"
      | isNot b            = " sagt die Wahrheit"
      |            isNot c = " lügt"
      | otherwise          = " sagen die Wahrheit"

    nameFromLeaf :: SynTree BinOp Char -> String
    nameFromLeaf (Leaf name)       = [name]
    nameFromLeaf (Not (Leaf name)) = [name]
    nameFromLeaf _                 = error "not a Leaf or not a Not Leaf"

    isNot :: SynTree b c -> Bool
    isNot (Not _) = True
    isNot _       = False

hintFromFormula _ = error "formula not supported"
