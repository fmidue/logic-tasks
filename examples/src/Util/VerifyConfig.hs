{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Util.VerifyConfig where

import Control.OutputCapable.Blocks (LangM, OutputCapable)
import Test.Hspec
import LogicTasks.Debug (checkConfigWith)
import Type.Reflection

verifyConfig :: a -> (forall m. OutputCapable m => a -> LangM m) -> Spec
verifyConfig config checker = itIsValid $ checkConfigWith config checker `shouldReturn` True

noChecker :: forall a. Typeable a => a -> Spec
noChecker _ = itIsValid $ pendingWith $ "no checker for " ++ show (typeRep @a)

itIsValid :: Example a => a -> SpecWith (Arg a)
itIsValid = it "is a valid configuration"
