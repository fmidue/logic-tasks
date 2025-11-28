{-# language NamedFieldPuns #-}

module SettingsGen where

import Test.QuickCheck.Gen


import Horn
import Trees.Types (SynTree(..), BinOp(..))



data Settings = Settings {
  spirit                   :: String,
  extra                    :: Int,
  showSolution             :: Bool,
  printFeedbackImmediately :: Bool,
  minimumPoints            :: Double
  } deriving (Eq,Show)


rollSettings :: Gen Settings
rollSettings = do
  (synTree,spirit) <- elements [(v1,"v1"),(v2,"v2")]
  extra <- chooseInt (0, length $ getAllAtomics synTree)
  showSolution <- chooseAny
  printFeedbackImmediately <- chooseAny
  minimumPoints <- genDouble
  pure $ Settings {
    spirit,
    extra,
    showSolution,
    printFeedbackImmediately,
    minimumPoints
    }

