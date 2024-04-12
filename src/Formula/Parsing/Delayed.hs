{-# LANGUAGE FlexibleContexts #-}
module Formula.Parsing.Delayed (Delayed, delayed, parseDelayed, parseDelayedRaw, withDelayed) where

import Text.Parsec
import Text.Parsec.String (Parser)
import ParsingHelpers (fully)

import Control.Monad.Output (LangM, english, german, OutputMonad)

import LogicTasks.Helpers (reject)

newtype Delayed a = Delayed String

delayed :: String -> Delayed a
delayed = Delayed

parseDelayed :: Parser a -> Delayed a -> Either ParseError a
parseDelayed = parseDelayedRaw

parseDelayedRaw :: Parser b -> Delayed a -> Either ParseError b
parseDelayedRaw p (Delayed str) = parse p "(answer string)" str

withDelayed :: OutputMonad m => (a -> LangM m) -> Parser a -> Delayed a -> LangM m
withDelayed grade p d =
  case parseDelayed (fully p) d of
    Left err -> reject $ do
      english $ show err
      german $ show err
    Right x -> grade x
