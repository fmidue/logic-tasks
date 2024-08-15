{-# LANGUAGE FlexibleContexts #-}
module Formula.Parsing.Delayed (Delayed, delayed, withDelayed, parseDelayedAndThen, complainAboutMissingParenthesesIfNotFailingOn) where

import Text.Parsec (ParseError, parse)
import Text.Parsec.String (Parser)
import Formula.Parsing (Parse(..))
import ParsingHelpers (fully)

import Control.OutputCapable.Blocks (LangM, Language, OutputCapable, english, german)
import Control.Monad.State (State)
import Data.Map (Map)

import LogicTasks.Helpers (reject)
import Formula.Parsing.Delayed.Internal (Delayed(..))

delayed :: String -> Delayed a
delayed = Delayed

parseDelayed :: Parser a -> Delayed a -> Either ParseError a
parseDelayed = parseDelayedRaw

parseDelayedRaw :: Parser b -> Delayed a -> Either ParseError b
parseDelayedRaw p (Delayed str) = parse p "(answer string)" str

withDelayed :: OutputCapable m => (a -> LangM m) -> Parser a -> Delayed a -> LangM m
withDelayed grade p d =
  case parseDelayed (fully p) d of
    Left err -> reject $ do
      english $ show err
      german $ show err
    Right x -> grade x

parseDelayedAndThen ::
  (OutputCapable m, Parse a)
  => (Maybe ParseError -> ParseError -> State (Map Language String) ())
  -> Parser ()
  -> (a -> LangM m)
  -> Delayed a
  -> LangM m
parseDelayedAndThen messaging fallBackParser whatToDo delayedAnswer =
  either
  (reject . messaging (either Just (const Nothing) $ parseDelayedRaw (fully fallBackParser) delayedAnswer))
  whatToDo
  (parseDelayed (fully parser) delayedAnswer)

complainAboutMissingParenthesesIfNotFailingOn :: Maybe a -> ParseError -> State (Map Language String) ()
complainAboutMissingParenthesesIfNotFailingOn maybeHereError latentError =
  case maybeHereError of
      Just _ -> do
        german $ show latentError
        english $ show latentError
      Nothing -> do
        german $  unlines
          [ "Ihre Abgabe konnte nicht gelesen werden." {- german -}
          , "Bitte prüfen Sie, ob die Anordnung der Symbole den Regeln zur Wohlaufgebautheit der Eingaben genügt." {- german -}
          , "Insbesondere sollten Sie genügend Klammern benutzen." {- german -}
          ]
        english $ unlines
          [ "Unable to read solution."
          , "Please make sure that the arrangement of symbols adheres to the rules for well-formed inputs."
          , "In particular, you should use enough parentheses."
          ]
