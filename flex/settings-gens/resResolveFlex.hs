module SettingsGen where

import Test.QuickCheck.Gen


newtype Settings = Settings {
  prefillSelect :: (Bool, Bool, Bool)
} deriving (Eq,Show)


rollSettings :: Gen Settings
rollSettings = Settings <$> chooseAny
