{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}

module LogicTasks.Helpers where


import qualified Data.ByteString as BS (readFile, writeFile)

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.OutputCapable.Blocks (
  GenericOutputCapable (..),
  LangM,
  Language,
  OutputCapable,
  english,
  german,
  translate,
  translations,
  translatedCode,
  localise,
  )
import Control.Monad.State (State, put)
import Data.Map (Map)
import Data.ByteString.Lazy (fromStrict)
import Data.ByteString.UTF8 (fromString)
import Data.Digest.Pure.SHA (sha256, showDigest)
import System.Directory (doesFileExist)



extra :: OutputCapable m => Maybe (Map Language String) -> LangM m
extra (Just extraMap) = paragraph $ translate $ put extraMap
extra _ = pure ()

indexed :: [String] -> [String]
indexed = zipWith (\a b -> show a ++ ". " ++ b) ([1..] :: [Int])



instruct :: OutputCapable m => State (Map Language String) () -> LangM m
instruct = paragraph . translate



focus :: OutputCapable m => String -> LangM m
focus = indent . code



example :: OutputCapable m => String -> State (Map Language String) () -> LangM m
example correct s = indent $ do
    instruct s
    code correct
    pure ()


reject :: OutputCapable m => State (Map Language String) () -> LangM m
reject  = refuse . indent . translate



clauseKey :: OutputCapable m => LangM m
clauseKey = do
  keyHeading
  negationKey
  orKey
  pure()

cnfKey :: OutputCapable m => LangM m
cnfKey = do
  clauseKey
  andKey
  pure ()

formulaKey :: OutputCapable m => LangM m
formulaKey = do
  keyHeading
  basicOpKey
  pure ()

basicOpKey :: OutputCapable m => LangM m
basicOpKey = do
  negationKey
  andKey
  orKey
  pure()

keyHeading :: OutputCapable m => LangM m
keyHeading =
  paragraph $ translate $ do
    german "Beachten Sie dabei die folgenden möglichen Schreibweisen:"
    english "You can use any of the following notations:"

andKey :: OutputCapable m => LangM m
andKey =
  paragraph $ indent $ do
    translate $ do
      german "Und:"
      english "And:"
    translatedCode $ flip localise $ translations $ do
      german "/\\, und"
      english "/\\, and"
    pure ()

orKey :: OutputCapable m => LangM m
orKey =
  paragraph $ indent $ do
    translate $ do
      german "Oder:"
      english "Or:"
    translatedCode $ flip localise $ translations $ do
      german "\\/, oder"
      english "\\/, or"
    pure ()


negationKey :: OutputCapable m => LangM m
negationKey =
  paragraph $ indent $ do
    text "Negation:"
    translatedCode $ flip localise $ translations $ do
      german "-, ~, nicht"
      english "-, ~, not"
    pure ()

arrowsKey :: OutputCapable m => LangM m
arrowsKey = do
  paragraph $ indent $ do
    translate $ do
      english "Implication:"
      german "Implikation:"
    code "=>, <="
    pure ()
  paragraph $ indent $ do
    translate $ do
      english "Bi-Implication:"
      german "Bi-Implikation:"
    code "<=>"
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
