{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Util.VerifyConfig where

import Control.OutputCapable.Blocks (LangM, OutputCapable, Language)
import Test.Hspec
import Type.Reflection
import Control.OutputCapable.Blocks.Debug (checkConfigWith)

verifyConfig :: Language -> a -> (forall m. OutputCapable m => a -> LangM m) -> Spec
verifyConfig lang config checker = itIsValid $ checkConfigWith lang config checker `shouldReturn` True

noChecker :: forall a. Typeable a => a -> Spec
noChecker _ = itIsValid $ pendingWith $ "no checker for " ++ show (typeRep @a)

itIsValid :: Example a => a -> SpecWith (Arg a)
itIsValid = it "is a valid configuration"
