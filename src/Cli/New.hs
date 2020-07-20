{-# LANGUAGE OverloadedStrings #-}

module Cli.New where

import Data.Convertible.Utf8 (convert)
import Turtle (shell, empty)
import Cli.Result
import Data.Text (Text)
import qualified Settings.Template as Template
import qualified Settings
import System.FilePath        ((</>))

getHsfilesPath :: IO FilePath
getHsfilesPath = do
  settingsDir <- Settings.getSettingsDir
  pure (settingsDir </> "atcoder-template.hsfiles")

new :: Text -> Result ()
new contest = do
  config <- Settings.getConfig
  let templateHsfiles = convert $ Template.atcoderTemplate config
  hsfilesPath <- liftIO getHsfilesPath
  liftIO $ writeFile hsfilesPath templateHsfiles
  -- create project
  shell
    ( "stack new "
        <> contest
        <> " " <> convert hsfilesPath
    )
    empty
  -- init build
  shell ( "cd " <> contest <> " && stack build --fast") empty
  liftIO $ putStrLn $ convert ("Created a new project to ./" <> contest)

templateHasFiles :: Result Text
templateHasFiles = Template.atcoderTemplate <$> Settings.getConfig
