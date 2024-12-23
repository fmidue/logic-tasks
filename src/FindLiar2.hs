{-# language QuasiQuotes #-}
{-# language OverloadedStrings #-}

module FindLiar2 where

import Data.Text (Text, unpack)
import Data.String.Interpolate (i)
import Trees.Types (BinOp(..), SynTree(..))
import Trees.Print (simplestDisplay)-- Testing

makeHintsAndFormula :: ((Char, Bool), (Char, Bool), (Char, Bool)) -> ([SynTree BinOp Char], [Text])
makeHintsAndFormula ((xn, False), (yn, False), (zn, zw)) = (parts, hints)
  where
    xZnOrNotZn = if zw then Leaf zn else Not (Leaf zn)

    yZnOrNotZn = if zw then Not (Leaf zn) else Leaf zn

    zXnOrNotXn = if zw then Not (Leaf xn) else Leaf xn
    zYnOrNotYn = if zw then Not (Leaf yn) else Leaf yn

    xOperator :: String = if isNot (Leaf yn) == isNot xZnOrNotZn then " und" else ", aber"
    xSdwLgt1 :: String = if xOperator == " und" then "" else sdW
    xSdwLgt2 :: String
      | xOperator == " und" = sdW2
      | otherwise = " lügt"

    yOperator :: String = if isNot (Leaf xn) == isNot yZnOrNotZn then " und" else ", aber"
    ySdWLgt1 :: String = if xOperator == " und" then sdW else ""
    ySdWLgt2 :: String
      | yOperator == " und" = sdW2
      | otherwise           = " lügt"

    zSdwLgt2 :: String
      | not zw    = sdW2
      | otherwise = " lügen"

    parts = [px, py, pz]
    px = Binary Equi (Leaf xn) (Binary And (Leaf yn) xZnOrNotZn)
    py = Binary Equi (Leaf yn) (Binary And (Leaf xn) yZnOrNotZn)
    pz = Binary Equi (Leaf zn) (Binary Or zXnOrNotXn zYnOrNotYn)

    hints = [hx, hy, hz]
    hx = [i|#{xn} sagt: "#{yn}#{xSdwLgt1}#{xOperator} #{zn}#{xSdwLgt2}."|]
    hy = [i|#{yn} sagt: "#{xn}#{ySdWLgt1}#{yOperator} #{zn}#{ySdWLgt2}."|]
    hz = [i|#{zn} sagt: "#{xn} oder #{yn}#{zSdwLgt2}."|]

    isNot :: SynTree b c -> Bool
    isNot (Not _) = True
    isNot _       = False

    sdW :: String
    sdW = " sagt die Wahrheit"

    sdW2 :: String
    sdW2 = " sagen die Wahrheit"

makeHintsAndFormula ((_, _), (_, _), (_, _)) = error "not supported yet"

-- Testing
main :: IO ()
main = do
    let (formulas, hints) = makeHintsAndFormula (('A', False), ('B', False), ('C', True))
    putStrLn "\nFormulas:"
    mapM_ (print . simplestDisplay) formulas
    putStrLn "\nHints:"
    mapM_ (putStrLn . unpack) hints
