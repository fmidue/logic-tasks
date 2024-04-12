{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
module Formula.Parsing.Delayed where


import Text.Parsec
import Text.Parsec.String (Parser)
import ParsingHelpers (fully)

import Control.Monad.Output (LangM, english, german, OutputMonad)

import LogicTasks.Helpers (reject)

import Data.Typeable (Typeable)
import GHC.Generics (Generic)

newtype Delayed a = Delayed String deriving (Eq, Show, Typeable, Generic)

delayed :: String -> Delayed a
delayed = Delayed

parseDelayed :: Delayed a -> Parser a -> Either ParseError a
parseDelayed (Delayed str) p = parse p "(delayed input)" str

withDelayed :: OutputMonad m => (a -> LangM m) -> Parser a -> Delayed a -> LangM m
withDelayed grade p d =
  case parseDelayed d (fully p) of
    Left err -> reject $ do
      english $ show err
      german $ show err
    Right x -> grade x
