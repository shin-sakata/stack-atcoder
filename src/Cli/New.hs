{-# LANGUAGE OverloadedStrings #-}

module Cli.New where

import           Cli.Result
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Convertible.Utf8  (convert)
import           Data.Text              (Text)
import qualified Settings
import qualified Settings.Template      as Template
import           System.FilePath        ((</>))
import           Turtle                 (empty, shell)

getHsfilesPath :: IO FilePath
getHsfilesPath = do
  settingsDir <- Settings.getSettingsDir
  pure (settingsDir </> "atcoder-template.hsfiles")

new :: (MonadThrow m, MonadIO m) => Text -> m ()
new contest = do
  config <- Settings.getConfig
  let templateHsfiles = convert $ Template.atcoderTemplate config
  hsfilesPath <- liftIO getHsfilesPath
  liftIO $ writeFile hsfilesPath templateHsfiles
  -- create project
  shell
    ( "stack new "
        <> contest
        <> " "
        <> convert hsfilesPath
    )
    empty
  -- init build
  shell ("cd " <> contest <> " && stack build --fast") empty
  liftIO $ putStrLn $ convert ("Created a new project to ./" <> contest)

templateHasFiles :: (MonadThrow m, MonadIO m) => m Text
templateHasFiles = Template.atcoderTemplate <$> Settings.getConfig
