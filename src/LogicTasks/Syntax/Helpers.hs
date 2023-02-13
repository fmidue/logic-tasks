
module LogicTasks.Syntax.Helpers where


import qualified Data.ByteString as BS (readFile, writeFile)

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Output (LangM, OutputMonad (..), english, german, translate)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.UTF8 (fromString)
import Data.Digest.Pure.SHA (sha256, showDigest)
import System.Directory (doesFileExist)




indexed :: [String] -> [String]
indexed = zipWith (\a b -> show a ++ ". " ++ b) ([1..] :: [Int])



bilingual :: OutputMonad m => String -> String -> LangM m
bilingual e g =
    translate $ do
      german g
      english e



instruct :: OutputMonad m => String -> String -> LangM m
instruct e g = paragraph $ bilingual e g



focus :: OutputMonad m => String -> LangM m
focus = indent . code



example :: OutputMonad m => String -> String -> String -> LangM m
example e g correct = indent $ do
      instruct e g
      code correct



reject :: OutputMonad m => String -> String -> LangM m
reject e g  = refuse $ indent $ bilingual e g



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
