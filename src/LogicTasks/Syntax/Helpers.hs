
module LogicTasks.Syntax.Helpers where


import Control.Monad.Output (LangM, OutputMonad (..), english, german, translate)




indexed :: [String] -> [String]
indexed = zipWith (\a b -> show a ++ ". " ++ b) ([1..] :: [Int])



bilingual :: OutputMonad m => String -> String -> LangM m
bilingual e g =
    translate $ do
      german g
      english e



instruct :: OutputMonad m => String -> String -> LangM m
instruct e g = paragraph $ bilingual e g



focus :: OutputMonad m => String -> LangM m
focus = indent . code



reject :: OutputMonad m => String -> String -> LangM m
reject e g  = refuse $ indent $ bilingual e g
