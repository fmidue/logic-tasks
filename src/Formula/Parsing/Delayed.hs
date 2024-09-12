{-# LANGUAGE FlexibleContexts #-}
module Formula.Parsing.Delayed (
  Delayed,
  delayed,
  withDelayed,
  displayParseError,
  withDelayedSucceeding,
  parseDelayedWithAndThen,
  complainAboutMissingParenthesesIfNotFailingOn,
  complainAboutWrongNotation
  ) where

import Text.Parsec (ParseError, parse)
import Text.Parsec.String (Parser)
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

withDelayed ::
  OutputCapable m
  => (a -> LangM m)
  -> Parser a
  -> (ParseError -> State (Map Language String) ())
  -> Delayed a
  -> LangM m
withDelayed whatToDo p displayError delayedAnswer =
  case parseDelayed (fully p) delayedAnswer of
    Left err -> reject (displayError err)
    Right x -> whatToDo x

displayParseError :: ParseError -> State (Map Language String) ()
displayParseError err = do
  english $ show err
  german $ show err

parseDelayedWithAndThen ::
  OutputCapable m
  => Parser a
  -> (Maybe ParseError -> ParseError -> State (Map Language String) ())
  -> Parser ()
  -> (a -> LangM m)
  -> Delayed a
  -> LangM m
parseDelayedWithAndThen p messaging fallBackParser whatToDo delayedAnswer =
  (whatToDo `withDelayed` p)
  (messaging (either Just (const Nothing) $
              parseDelayedRaw (fully fallBackParser) delayedAnswer))
  delayedAnswer

withDelayedSucceeding ::
  OutputCapable m
  => (a -> LangM m)
  -> Parser a
  -> Delayed a
  -> LangM m
withDelayedSucceeding whatToDo p delayedAnswer =
  case parseDelayed (fully p) delayedAnswer of
    Left err -> error $ "It should be impossible here, and yet the following ParseError was encountered: " ++ show err
    Right x -> whatToDo x

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
          [ "Unable to read submission."
          , "Please make sure that the arrangement of symbols adheres to the rules for well-formed inputs."
          , "In particular, you should use enough parentheses."
          ]

complainAboutWrongNotation :: State (Map Language String) ()
complainAboutWrongNotation = do
  german $ unlines
    [ "Ihre Abgabe konnte nicht gelesen werden." {- german -}
    , "Bitte stellen Sie sicher, dass Sie die geforderte Notation verwenden." {- german -}
    ]
  english $ unlines
    [ "Unable to read submission."
    , "Please make sure to use the required notation."
    ]
