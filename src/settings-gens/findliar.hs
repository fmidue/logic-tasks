{-# language NamedFieldPuns #-}

module SettingsGen where

import Test.QuickCheck.Gen


data Settings = Settings {
  emptyColumns             :: Int,
  staticColumns            :: Int,
  staticColsEnd            :: Int,
  showSolution             :: Bool,
  printFeedbackImmediately :: Bool
  } deriving (Eq,Show)


rollSettings :: Gen Settings
rollSettings = do
  (emptyColumns, staticColumns, staticColsEnd) <- do
    a <- chooseInt (1,16)
    b <- chooseInt (1,17-a)
    c <- chooseInt (1,18-a-b)
    pure (a,b,c)
  showSolution <- chooseAny
  printFeedbackImmediately <- chooseAny
  pure $ Settings { 
    emptyColumns,
    staticColumns,
    staticColsEnd,
    showSolution,
    printFeedbackImmediately
    }

