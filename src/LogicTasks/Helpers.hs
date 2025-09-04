{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}

module LogicTasks.Helpers where


import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  Language,
  OutputCapable,
  english,
  german,
  translate,
  translations,
  translatedCode,
  localise,
  )
import Control.Monad.State (State, put)
import Data.Map (Map)



extra :: OutputCapable m => Maybe (Map Language String) -> LangM m
extra (Just extraMap) = paragraph $ translate $ put extraMap
extra _ = pure ()

indexed :: [String] -> [String]
indexed = zipWith (\a b -> show a ++ ". " ++ b) ([1..] :: [Int])



instruct :: OutputCapable m => State (Map Language String) () -> LangM m
instruct = paragraph . translate



focus :: OutputCapable m => String -> LangM m
focus = indent . code



example :: OutputCapable m => String -> State (Map Language String) () -> LangM m
example correct s = indent $ do
    instruct s
    code correct
    pure ()


reject :: OutputCapable m => State (Map Language String) () -> LangM m
reject  = refuse . indent . translate



clauseKey :: OutputCapable m => Bool -> LangM m
clauseKey allowUnicode = do
  keyHeading
  negationKey allowUnicode
  orKey allowUnicode
  pure()

cnfKey :: OutputCapable m => Bool -> LangM m
cnfKey allowUnicode = do
  clauseKey allowUnicode
  andKey allowUnicode
  pure ()

formulaKey :: OutputCapable m => Bool -> LangM m
formulaKey allowUnicode = do
  keyHeading
  basicOpKey allowUnicode
  pure ()

basicOpKey :: OutputCapable m => Bool -> LangM m
basicOpKey allowUnicode = do
  negationKey allowUnicode
  andKey allowUnicode
  orKey allowUnicode
  pure()

keyHeading :: OutputCapable m => LangM m
keyHeading =
  paragraph $ translate $ do
    german "Beachten Sie dabei die folgenden möglichen Schreibweisen:"
    english "You can use any of the following notations:"

andKey :: OutputCapable m => Bool -> LangM m
andKey allowUnicode =
  paragraph $ indent $ do
    translate $ do
      german "Und:"
      english "And:"
    translatedCode $ flip localise $ translations $ do
      german $ (if allowUnicode then "∧, " else "") ++ "/\\, und"
      english $ (if allowUnicode then "∧, " else "") ++ "/\\, and"
    pure ()

orKey :: OutputCapable m => Bool -> LangM m
orKey allowUnicode =
  paragraph $ indent $ do
    translate $ do
      german "Oder:"
      english "Or:"
    translatedCode $ flip localise $ translations $ do
      german $ (if allowUnicode then "∨, " else "") ++ "\\/, oder"
      english $ (if allowUnicode then "∨, " else "") ++ "\\/, or"
    pure ()


negationKey :: OutputCapable m => Bool -> LangM m
negationKey allowUnicode =
  paragraph $ indent $ do
    text "Negation:"
    translatedCode $ flip localise $ translations $ do
      german $ (if allowUnicode then "¬, " else "") ++ "-, ~, nicht"
      english $ (if allowUnicode then "¬, " else "") ++ "-, ~, not"
    pure ()

arrowsKey :: OutputCapable m => LangM m
arrowsKey = do
  paragraph $ indent $ do
    translate $ do
      english "Implication:"
      german "Implikation:"
    code "=>, <="
    pure ()
  paragraph $ indent $ do
    translate $ do
      english "Bi-Implication:"
      german "Bi-Implikation:"
    code "<=>"
    pure ()
  pure ()
