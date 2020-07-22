{-# LANGUAGE OverloadedStrings #-}

module Cli.Submit where

import qualified AtCoder
import           Cli.Exceptions
import           Data.Convertible.Utf8 (convert)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           System.Directory      as Dir

submit :: (MonadThrow m, MonadIO m) => T.Text -> m ()
submit task = do
  contest <- liftIO getCurrentContest
  sourceCode <- liftIO $ getSourceCode task
  AtCoder.submit contest task sourceCode

getCurrentContest :: IO T.Text
getCurrentContest = do
  dir <- convert <$> Dir.getCurrentDirectory
  pure $ last (T.split (== '/') dir)

getSourceCode :: T.Text -> IO T.Text
getSourceCode task = T.readFile (convert task <> "/Main.hs")
