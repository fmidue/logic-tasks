{-# language QuasiQuotes #-}
{-# language OverloadedStrings #-}

module FindLiar2 where

import Data.Text (Text, unpack)
import Data.String.Interpolate (i)
import Trees.Types (BinOp(..), SynTree(..))
import Trees.Print (simplestDisplay)-- Testing

makeHintsAndFormula :: ((Char, Bool), (Char, Bool), (Char, Bool)) -> ([SynTree BinOp Char], [Text])
makeHintsAndFormula ((xn, xw), (yn, yw), (zn, zw)) = (parts, hints)
  where
    xYnOrNotYn = if xw == yw then Leaf yn else Not (Leaf yn)
    xZnOrNotZn = if xw == zw then Not (Leaf zn) else Leaf zn

    yXnOrNotXn = if xw == yw then Leaf xn else Not (Leaf xn)
    yZnOrNotZn = if yw == zw then Leaf zn else Not (Leaf zn)

    zXnOrNotXn = if xw == zw then Leaf xn else Not (Leaf xn)
    zYnOrNotYn = if yw == zw then Leaf yn else Not (Leaf yn)

    parts = [px, py, pz]
    px = Binary Equi (Leaf xn) (Binary And xYnOrNotYn xZnOrNotZn)
    py = Binary Equi (Leaf yn) (Binary And yXnOrNotXn yZnOrNotZn)
    pz = Binary Equi (Leaf zn) (Binary Or  zXnOrNotXn zYnOrNotYn)

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
    nameFromLeaf _                 = error "This is impossible!"

    isNot :: SynTree b c -> Bool
    isNot (Not _) = True
    isNot _       = False

hintFromFormula _ = error "formula not supported"


-- Testing
main :: IO ()
main = do
    let (formulas, hints) = makeHintsAndFormula (('A', False), ('B', False), ('C', False))
    putStrLn "\nFormulas:"
    mapM_ (print . simplestDisplay) formulas
    putStrLn "\nHints:"
    mapM_ (putStrLn . unpack) hints

    let (formulas2, hints2) = makeHintsAndFormula (('A', False), ('B', False), ('C', True))
    putStrLn "\nFormulas:"
    mapM_ (print . simplestDisplay) formulas2
    putStrLn "\nHints:"
    mapM_ (putStrLn . unpack) hints2

    let (formulas3, hints3) = makeHintsAndFormula (('A', False), ('B', True), ('C', False))
    putStrLn "\nFormulas:"
    mapM_ (print . simplestDisplay) formulas3
    putStrLn "\nHints:"
    mapM_ (putStrLn . unpack) hints3

    let (formulas4, hints4) = makeHintsAndFormula (('A', False), ('B', True), ('C', True))
    putStrLn "\nFormulas:"
    mapM_ (print . simplestDisplay) formulas4
    putStrLn "\nHints:"
    mapM_ (putStrLn . unpack) hints4

    let (formulas5, hints5) = makeHintsAndFormula (('A', True), ('B', False), ('C', False))
    putStrLn "\nFormulas:"
    mapM_ (print . simplestDisplay) formulas5
    putStrLn "\nHints:"
    mapM_ (putStrLn . unpack) hints5
