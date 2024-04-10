{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Formula.Parsing.Compat where

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Pos as Parsec

import qualified Text.Megaparsec as Megaparsec
import ParsingHelpers as Megaparsec (Parser, fully)
import Text.Megaparsec (ParseErrorBundle(..), ParseError (..), ErrorItem(..))

import Control.Monad.Output (LangM, english, german, OutputMonad)

import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Set as Set
import Data.Void (Void)

import Formula.Parsing (Parse(parser))
import LogicTasks.Helpers (reject)

newtype Delayed a = Delayed { parseDelayed :: Megaparsec.Parser a -> Either (ParseErrorBundle String Void) a }

withDelayed :: OutputMonad m => (a -> LangM m) -> Megaparsec.Parser a -> Delayed a -> LangM m
withDelayed grade p d =
  case parseDelayed d (fully p) of
    Left err -> reject $ do
      english $ Megaparsec.errorBundlePretty $ filterErrors err
      german $ Megaparsec.errorBundlePretty $ filterErrors err
    Right x -> grade x

delayedParser :: Parsec.Parsec String () (Delayed a)
delayedParser = (\str -> Delayed $ \p -> Megaparsec.parse p "(input)" str) <$> Parsec.many Parsec.anyChar

direct :: Parse a => Parsec.Parsec String () a
direct = do
  str <- Parsec.getInput
  pos <- Parsec.getPosition
  case Megaparsec.parse
        ( do
          st <- Megaparsec.getParserState
          Megaparsec.setParserState st{ Megaparsec.stateInput = str, Megaparsec.statePosState = (Megaparsec.statePosState st){Megaparsec.pstateSourcePos = convertPos pos} }
          parseWithPosAndRest
        )
      "(megaparsec-input)" str of
    Left err -> fail $ Megaparsec.errorBundlePretty $ filterErrors err
    Right (x,newPos,rest) -> do
      Parsec.setInput rest
      Parsec.setPosition $ convertMegaPos newPos
      pure x
  where
    parseWithPosAndRest :: Parse a => Megaparsec.Parser (a,Megaparsec.SourcePos,String)
    parseWithPosAndRest = do
      res <- parser
      pos <- Megaparsec.getSourcePos
      rest <- Megaparsec.getInput
      pure (res,pos,rest)

    convertMegaPos :: Megaparsec.SourcePos -> Parsec.SourcePos
    convertMegaPos (Megaparsec.SourcePos n l c) = Parsec.newPos n (Megaparsec.unPos l) (Megaparsec.unPos c)

    convertPos :: Parsec.SourcePos -> Megaparsec.SourcePos
    convertPos pos = Megaparsec.SourcePos (Parsec.sourceName pos) (Megaparsec.mkPos $ Parsec.sourceLine pos) (Megaparsec.mkPos $ Parsec.sourceColumn pos)


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
