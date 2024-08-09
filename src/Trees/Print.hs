{-# OPTIONS_GHC -Wno-orphans #-}
module Trees.Print (
  transferToPicture,
  display,
  normalShow,
  simplestDisplay,
  ) where

import Trees.Types (SynTree(..), BinOp(..), showOperator, showOperatorNot)
import Text.PrettyPrint.Leijen.Text (Pretty(..), text, vcat, char)
import Data.Text.Lazy (pack)

transferToPicture :: SynTree BinOp Char -> String
transferToPicture (Binary And a b) = "[ $\\wedge $ " ++ transferToPicture a ++ transferToPicture b ++ "  ]"
transferToPicture (Leaf a) = "[" ++ (a:"]")
transferToPicture (Binary Or a b) = "[ $\\vee   $ " ++ transferToPicture a ++ transferToPicture b ++ "  ]"
transferToPicture (Not a) = "[ $\\neg $ " ++ transferToPicture a ++ "  ]"
transferToPicture (Binary Impl a b) = "[ $\\Rightarrow  $ " ++ transferToPicture a ++ transferToPicture b ++ "  ]"
transferToPicture (Binary Equi a b) = "[ $\\Leftrightarrow  $ " ++ transferToPicture a ++ transferToPicture b ++"  ]"
transferToPicture (Binary BackImpl a b) = "[ $\\Leftarrow  $ " ++ transferToPicture a ++ transferToPicture b ++ "  ]"

display :: SynTree BinOp Char -> String
display (Binary operator a b) = normalShow a ++ " " ++ showOperator operator ++ " " ++ normalShow b
display (Leaf a) = a : ""
display (Not a) = showOperatorNot ++ normalShow a

normalShow :: SynTree BinOp Char -> String
normalShow (Binary operator a b) = "(" ++ normalShow a ++ " " ++ showOperator operator ++ " " ++ normalShow b ++ ")"
normalShow (Leaf a) = a : ""
normalShow (Not a) = showOperatorNot ++ normalShow a

simplestDisplay :: SynTree BinOp Char -> String
simplestDisplay (Leaf a)=  a : ""
simplestDisplay (Not a) = showOperatorNot ++ simplestShow a Nothing
simplestDisplay (Binary operator a b) =
    simplestShow a (Just operator) ++ " " ++ showOperator operator ++ " " ++ simplestShow b (Just operator)

simplestShow :: SynTree BinOp Char -> Maybe BinOp -> String
simplestShow (Leaf a) _ =  a : ""
simplestShow (Not a) _ = showOperatorNot ++ simplestShow a Nothing
simplestShow (Binary And a b) j@(Just And) = simplestShow a j ++ " " ++ showOperator And ++ " " ++ simplestShow b j
simplestShow (Binary Or a b) j@(Just Or) = simplestShow a j ++ " " ++ showOperator Or ++ " " ++ simplestShow b j
simplestShow (Binary operator a b) _ =
  let
    j = Just operator
  in
    "(" ++ simplestShow a j ++ " " ++ showOperator operator ++ " " ++ simplestShow b j ++ ")"

instance Pretty BinOp where
  pretty = text . pack . showOperator

instance (Pretty o, Pretty c) => Pretty (SynTree o c) where
  pretty (Leaf x) = pretty x
  pretty (Not x) = text (pack showOperatorNot) <> pretty x
  pretty (Binary op l r) = vcat [ char '('
                                , pretty l
                                , text $ pack ") "
                                , pretty op
                                , text $ pack " ("
                                , pretty r
                                , char ')'
                                ]
