module Trees.Print (
  transferToPicture,
  display,
  normalShow,
  simplestDisplay,
  ) where

import Trees.Types (SynTree(..), BinOp(..), showOperator, showOperatorNot)

transferToPicture :: SynTree BinOp Char -> String
transferToPicture (Binary And a b) = "[ $\\wedge $ " ++ transferToPicture a ++ transferToPicture b ++ "  ]"
transferToPicture (Leaf a) = "[" ++ (a:"]")
transferToPicture (Binary Or a b) = "[ $\\vee   $ " ++ transferToPicture a ++ transferToPicture b ++ "  ]"
transferToPicture (Not a) = "[ $\\neg $ " ++ transferToPicture a ++ "  ]"
transferToPicture (Binary Impl a b) = "[ $\\to  $ " ++ transferToPicture a ++ transferToPicture b ++ "  ]"
transferToPicture (Binary Equi a b) = "[ $\\Leftrightarrow  $ " ++ transferToPicture a ++ transferToPicture b ++"  ]"

display :: SynTree BinOp Char -> String
display (Binary oper a b) = normalShow a ++ " " ++ showOperator oper ++ " " ++ normalShow b
display (Leaf a) = a : ""
display (Not a) = showOperatorNot ++ normalShow a

normalShow :: SynTree BinOp Char -> String
normalShow (Binary oper a b) = "(" ++ normalShow a ++ " " ++ showOperator oper ++ " " ++ normalShow b ++ ")"
normalShow (Leaf a) = a : ""
normalShow (Not a) = showOperatorNot ++ normalShow a

simplestDisplay :: SynTree BinOp Char -> String
simplestDisplay (Leaf a)=  a : ""
simplestDisplay (Not a) = showOperatorNot ++ simplestShow a Nothing
simplestDisplay (Binary oper a b) =
    simplestShow a (Just oper) ++ " " ++ showOperator oper ++ " " ++ simplestShow b (Just oper)

simplestShow :: SynTree BinOp Char -> Maybe BinOp -> String
simplestShow (Leaf a) _ =  a : ""
simplestShow (Not a) _ = showOperatorNot ++ simplestShow a Nothing
simplestShow (Binary And a b) j@(Just And) = simplestShow a j ++ " " ++ showOperator And ++ " " ++ simplestShow b j
simplestShow (Binary Or a b) j@(Just Or) = simplestShow a j ++ " " ++ showOperator Or ++ " " ++ simplestShow b j
simplestShow (Binary oper a b) _ =
  let
    j = Just oper
  in
    "(" ++ simplestShow a j ++ " " ++ showOperator oper ++ " " ++ simplestShow b j ++ ")"
