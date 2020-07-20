{-# LANGUAGE OverloadedStrings #-}

module Cli.Submit where

import qualified AtCoder
import Data.Convertible.Utf8 (convert)
import qualified Data.Text as T
import System.Directory as Dir
import qualified Data.Text.IO as T
import Cli.Result

submit :: T.Text -> Result ()
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
