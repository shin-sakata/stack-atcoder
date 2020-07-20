{-# LANGUAGE OverloadedStrings #-}

module Cli.New where

import Data.Convertible.Utf8 (convert)
import Turtle (shell, empty)
import qualified Settings
import Cli.Result
import Data.Text (Text)

resolver :: Text
resolver = " " <> "--resolver ghc-8.8.3"

templatePath :: Text
templatePath = " " <>  "~/Workspace/haskell/stack-atcoder/atcoder-template.hsfiles"

new :: Text -> Result ()
new contest = do
  -- create project
  shell
    ( "stack new "
        <> contest
        <> templatePath
        <> resolver
    )
    empty
  -- init build
  shell ( "cd " <> contest <> " && stack build --fast") empty
  liftIO $ putStrLn $ convert ("Created a new project to ./" <> contest)
