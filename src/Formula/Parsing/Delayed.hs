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

import Control.OutputCapable.Blocks (
  LangM,
  LangM',
  OutputCapable,
  english,
  german,
  indent,
  refuse,
  text,
  translate
  )

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
  -> (ParseError -> LangM m)
  -> Delayed a
  -> LangM m
withDelayed whatToDo p displayError delayedAnswer =
  case parseDelayed (fully p) delayedAnswer of
    Left err -> refuse $ indent $ displayError err
    Right x -> whatToDo x

displayParseError :: OutputCapable m => ParseError -> LangM m
displayParseError = text . show

parseDelayedWithAndThen ::
  OutputCapable m
  => Parser a
  -> (Maybe ParseError -> ParseError -> LangM m)
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
  => (a -> LangM' m b)
  -> Parser a
  -> Delayed a
  -> LangM' m b
withDelayedSucceeding whatToDo p delayedAnswer =
  case parseDelayed (fully p) delayedAnswer of
    Left err -> error $ "It should be impossible here, and yet the following ParseError was encountered: " ++ show err
    Right x -> whatToDo x

complainAboutMissingParenthesesIfNotFailingOn :: OutputCapable m => Maybe a -> ParseError -> LangM m
complainAboutMissingParenthesesIfNotFailingOn maybeHereError latentError =
  case maybeHereError of
      Just _ ->
        text $ show latentError
      Nothing ->
        translate $ do
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

complainAboutWrongNotation :: OutputCapable m => LangM m
complainAboutWrongNotation = translate $ do
  german $ unlines
    [ "Ihre Abgabe konnte nicht gelesen werden." {- german -}
    , "Bitte stellen Sie sicher, dass Sie die geforderte Notation verwenden." {- german -}
    ]
  english $ unlines
    [ "Unable to read submission."
    , "Please make sure to use the required notation."
    ]
