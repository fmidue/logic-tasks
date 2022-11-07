
module LogicTasks.Syntax.Helpers where


import Control.Monad.Output (LangM, OutputMonad (..), english, german, translate)




indexed :: [String] -> [String]
indexed = zipWith (\a b -> show a ++ ". " ++ b) ([1..] :: [Int])



instruct :: OutputMonad m => String -> String -> LangM m
instruct e g =
    paragraph $ translate $ do
      german g
      english e


focus :: OutputMonad m => String -> LangM m
focus = indent . code