{-# language QuasiQuotes #-}
{-# language OverloadedStrings #-}

module BuildHints where

import Data.Text (Text)
import Data.String.Interpolate (i)
import Trees.Types (BinOp(..), SynTree(..))

hintFromFormula :: SynTree BinOp Char -> Text
hintFromFormula (Binary Equi (Leaf nameA) (Binary operator b c)) =
  [i|#{nameA} sagt: "#{nameFromLeaf b}#{word1}#{operatorName} #{nameFromLeaf c}#{word2}."|]
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
    nameFromLeaf _                 = error "not a Leaf and not a Not Leaf"

hintFromFormula _ = error "formula not supported"

isNot :: SynTree b c -> Bool
isNot (Not _) = True
isNot _       = False
