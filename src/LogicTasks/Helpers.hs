{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}

module LogicTasks.Helpers where


import qualified Data.ByteString as BS (readFile, writeFile)

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Output (
  GenericOutputMonad (..),
  LangM,
  Language,
  OutputMonad,
  english,
  german,
  translate,
  )
import Control.Monad.State (State)
import Data.Map (Map)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.UTF8 (fromString)
import Data.Digest.Pure.SHA (sha256, showDigest)
import System.Directory (doesFileExist)




indexed :: [String] -> [String]
indexed = zipWith (\a b -> show a ++ ". " ++ b) ([1..] :: [Int])



instruct :: OutputMonad m => State (Map Language String) () -> LangM m
instruct = paragraph . translate



focus :: OutputMonad m => String -> LangM m
focus = indent . code



example :: OutputMonad m => String -> State (Map Language String) () -> LangM m
example correct s = indent $ do
    instruct s
    code correct
    pure ()


reject :: OutputMonad m => State (Map Language String) () -> LangM m
reject  = refuse . indent . translate



clauseKey :: OutputMonad m => LangM m
clauseKey = do
  paragraph $ translate $ do
    german "Beachten Sie dabei die folgende Legende:"
    english "Use the following key:"

  paragraph $ indent $ do
    text "Negation:"
    code "~"
    pure ()
  paragraph $ indent $ do
    translate $ do
      german "Oder:"
      english "Or:"
    code "\\/"
    pure ()
  pure()


cnfKey :: OutputMonad m => LangM m
cnfKey = do
  clauseKey
  paragraph $ indent $ do
    translate $ do
      german "Und:"
      english "And:"
    code "/\\"
    pure ()
  pure ()


cacheIO
  :: (MonadIO m, Show a)
  => FilePath
  -- ^ base file path (prefix of file name)
  -> String
  -- ^ path prefix (including dot and extension)
  -> String
  -- ^ some identifying name for what (part of file name)
  -> a
  -- ^ what
  -> (FilePath -> a -> m b)
  -- ^ how to create something from what
  -> m FilePath
cacheIO path ext name what how = (file <$) . cache $ how file what
  where
    cache create = do
      let create' = create >> liftIO (BS.writeFile whatFile what')
      isFile <- liftIO $ doesFileExist file
      if isFile
        then do
          f <- liftIO $ BS.readFile whatFile
          when (f /= what') $ do
            liftIO $ appendFile (path ++ "busted.txt") whatId
            create'
        else create'
    what' = fromString $ show what
    whatId = path ++ name ++ showDigest (sha256 $ fromStrict what')
    whatFile = whatId ++ ".hs"
    file = whatId ++ ext
