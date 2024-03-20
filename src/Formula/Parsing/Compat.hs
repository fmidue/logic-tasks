{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Formula.Parsing.Compat where

import qualified Text.Parsec as Parsec
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec (ParseErrorBundle(..), ParseError (..), ErrorItem(..))
import Data.Void (Void)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import ParsingHelpers as Megaparsec (Parser, fully)
import Text.Parsec (anyChar)
import Control.Monad.Output (LangM, english, german, OutputMonad)
import LogicTasks.Helpers (reject)
import qualified Data.Set as Set

newtype Delayed a = Delayed { parseDelayed :: Megaparsec.Parser a -> Either (ParseErrorBundle String Void) a }

withDelayed :: OutputMonad m => (a -> LangM m) -> Parser a -> Delayed a -> LangM m
withDelayed grade p d =
  case parseDelayed d (fully p) of
    Left err -> reject $ do
      english $ Megaparsec.errorBundlePretty $ filterErrors err
      german $ Megaparsec.errorBundlePretty $ filterErrors err
    Right x -> grade x

delayedParser :: Parsec.Parsec String () (Delayed a)
delayedParser = (\str -> Delayed $ \p -> Megaparsec.parse p "(input)" str) <$> (Parsec.many anyChar <* Parsec.eof)

-- filter out unhelpfull (parts of) error messages
-- (currently only "expecting white space" messages)
filterErrors :: ParseErrorBundle String Void -> ParseErrorBundle String Void
filterErrors (ParseErrorBundle es posSt) = ParseErrorBundle (NonEmpty.map cleanupError es) posSt
  where
    cleanupError (TrivialError off unexpt expt) = TrivialError off unexpt $ filterExpectations expt
    cleanupError e@FancyError{} = e

    filterExpectations expt = flip Set.filter expt $ \case
      Label (c :| cs) -> c:cs /= "white space"
      Tokens _ -> True
      EndOfInput -> True
