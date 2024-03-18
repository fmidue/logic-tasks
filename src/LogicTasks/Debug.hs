{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
module LogicTasks.Debug where

import Test.QuickCheck
import Control.Monad.Output.Generic
import Control.Monad.Output
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Maybe (isJust)
import Formula.Types (Cnf(..), Clause(..), Literal(..), Formula(..))
import Formula.Util (isPositive)
import Data.Set (size, toList)
import Data.List (partition)
import Data.List.Extra (nubSort)

showDescription :: (m ~ GenericReportT Language (IO ()) IO) => Gen inst -> (inst -> LangM m) -> IO (Maybe ())
showDescription gen f = do
  inst <- generate gen
  run (f inst)

testTask ::
  (m ~ GenericReportT Language (IO ()) IO, Show a) =>
  Gen inst ->
  (inst -> LangM m) ->
  (inst -> a -> LangM m) ->
  (inst -> a -> LangM m) ->
  Parser a ->
  IO ()
testTask gen f partial full p = do
  inst <- generate gen
  desc <- run (f inst)
  print desc
  str <- getLine
  case parse p "input" str of
    Left err -> print err
    Right value -> do
      putStrLn "---- Input ----"
      print value
      putStrLn "---- Partial ----"
      partialRes <- run (partial inst value)
      print partialRes
      putStrLn "---- Complete ----"
      completeRes <- run (full inst value)
      print completeRes

checkConfigWith :: (m ~ GenericReportT Language (IO ()) IO) => config -> (config -> LangM m) -> IO Bool
checkConfigWith conf check = isJust <$> run (check conf)

run :: (m ~ GenericReportT Language (IO ()) IO) => LangM m -> IO (Maybe ())
run thing = do
  (r,sayThing) <- runLangMReport (pure ()) (>>) thing
  sayThing German
  pure r

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
