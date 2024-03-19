{-# LANGUAGE FlexibleContexts #-}
module Formula.Parsing.Compat where

import qualified Text.Parsec as Parsec
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec (ParseErrorBundle)
import Data.Void (Void)
import ParsingHelpers as Megaparsec (Parser)
import Text.Parsec (anyChar)
import Control.Monad.Output (LangM, english, german, OutputMonad)
import LogicTasks.Helpers (reject)

newtype Delayed a = Delayed { parseDelayed :: Megaparsec.Parser a -> Either (ParseErrorBundle String Void) a }

withDelayed :: OutputMonad m => (a -> LangM m) -> Parser a -> Delayed a -> LangM m
withDelayed grade p d =
  case parseDelayed d p of
    Left err -> reject $ do
      english $ Megaparsec.errorBundlePretty err
      german $ Megaparsec.errorBundlePretty err
    Right x -> grade x

delayedParser :: Parsec.Parsec String () (Delayed a)
delayedParser = (\str -> Delayed $ \p -> Megaparsec.parse p "(input)" str) <$> (Parsec.many anyChar <* Parsec.eof)
