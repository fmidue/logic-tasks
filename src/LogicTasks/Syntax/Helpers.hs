
module LogicTasks.Syntax.Helpers where


import Data.Digest.Pure.SHA (sha256, showDigest)
import qualified Data.ByteString.Lazy             as LBS (fromStrict)
import Control.Monad.IO.Class           (MonadIO (liftIO))
import qualified Data.ByteString.UTF8             as BS (fromString)
import qualified Data.ByteString                  as BS (readFile, writeFile)
import Control.Monad                    (when)
import System.Directory                 (doesFileExist)
import Control.Monad.Output (LangM, OutputMonad (..), english, german, translate)




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
    what' = BS.fromString $ show what
    whatId = path ++ name ++ showDigest (sha256 $ LBS.fromStrict what')
    whatFile = whatId ++ ".hs"
    file = whatId ++ ext