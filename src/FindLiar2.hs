{-# language QuasiQuotes #-}
{-# language OverloadedStrings #-}

module FindLiar2 where

import Data.Text (Text, unpack)
import Data.String.Interpolate (i)
import Trees.Types (BinOp(..), SynTree(..))
import Trees.Print (simplestDisplay)-- Testing

makeHintsAndFormula :: ((Char, Bool), (Char, Bool), (Char, Bool)) -> ([SynTree BinOp Char], [Text])
makeHintsAndFormula ((xn, False), (yn, False), (zn, False)) = (parts, hints)
  where
    parts = [px, py, pz]
    px = Binary Equi (Leaf xn) (Binary And (Leaf yn) (Not (Leaf zn)))
    py = Binary Equi (Leaf yn) (Binary And (Leaf xn) (Leaf zn))
    pz = Binary Equi (Leaf zn) (Binary Or (Leaf xn) (Leaf yn))

    hints = [hx, hy, hz]
    hx = [i|#{xn} sagt: "#{yn}#{sdW}, aber #{zn} lügt."|]
    hy = [i|#{yn} sagt: "#{xn} und #{zn}#{sdW2}."|]
    hz = [i|#{zn} sagt: "#{xn} oder #{yn}#{sdW2}."|]

    sdW :: String
    sdW = " sagt die Wahrheit"

    sdW2 :: String
    sdW2 = " sagen die Wahrheit"

makeHintsAndFormula ((_, _), (_, _), (_, _)) = error "not supported yet"

-- Testing
main :: IO ()
main = do
    let (formulas, hints) = makeHintsAndFormula (('A', False), ('B', False), ('C', False))
    putStrLn "\nFormulas:"
    mapM_ (print . simplestDisplay) formulas
    putStrLn "\nHints:"
    mapM_ (putStrLn . unpack) hints
