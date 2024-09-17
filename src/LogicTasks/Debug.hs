{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module LogicTasks.Debug (
  testModule, analyseCnfGenerator, isTrivial,
  -- re-exports for (ghci) calls to testModule
  Display(..), Language (..),
  ) where

import Test.QuickCheck
import Control.OutputCapable.Blocks.Generic
import Control.OutputCapable.Blocks
import Text.Parsec
import Text.Parsec.String (Parser)
import Formula.Types (Cnf(..), Clause(..), Literal(..), Formula(..))
import Formula.Util (isPositive)
import Data.Set (size, toList)
import Data.List (partition)
import Data.List.Extra (nubSort)
import Control.OutputCapable.Blocks.Debug (testTask, Display(..))
import Formula.Parsing.Delayed (delayed)
import Formula.Parsing.Delayed.Internal (Delayed(..))
import Formula.Parsing (Parse(..))
import ParsingHelpers (fully, lexeme)

deriving instance Show (Delayed a)

instance Parse Int where
  parser = read <$> lexeme (many1 digit) <?> "Int"

instance Parse (Delayed a) where
  parser = delayed <$> fully (many anyChar)

testModule ::
  (m ~ GenericReportT Language (IO ()) IO, Show a, Show c) =>
  Maybe (Display a) ->
  Language ->
  Gen inst ->
  (inst -> LangM m) ->
  (inst -> a -> LangM m) ->
  (inst -> a -> LangM' m c) ->
  Parser a ->
  IO ()
testModule prettyCfg lang gen desc partial complete p =
  testTask prettyCfg lang (generate gen) desc partial complete (either (error . show) id . parse p "Input" <$> getLine)

analyseCnfGenerator :: Gen Cnf -> IO ()
analyseCnfGenerator gen = quickCheckWith stdArgs{maxSuccess=1000} $ forAll gen $ \cnf ->
  tabulate "all literals" (map show $ literals cnf) $
  tabulate "positive literals" (map show $ filter isPositive $ literals cnf) $
  tabulate "negative literals" (map show $ filter (not . isPositive) $ literals cnf) $
  tabulate "clause lengths" (map (show . size . literalSet) . toList $ clauseSet cnf) $
  tabulate "number of clauses" (pure . show . size $ clauseSet cnf) $
  tabulate "trivial clauses (containing both X and not X)" (map (show . isTrivial) . toList $ clauseSet cnf) $
  tabulate "usage of atomic propositions" (pure . nubSort . map (\case (Literal x) -> x ; (Not x) -> x) $ literals cnf)
    True

isTrivial :: Clause -> Bool
isTrivial x =
  let (pos,neg) = partition isPositive $ toList $ literalSet x
  in any (\case (Literal a) -> Not a `elem` neg; _ -> error "impossible") pos
