{-# LANGUAGE OverloadedStrings #-}

module Cli.New where

import Data.Convertible.Utf8 (convert)
import Turtle

resolver :: Text
resolver = " " <> "--resolver ghc-8.8.3"

templatePath :: Text
templatePath = " " <>  "./atcoder-template.hsfiles"

new :: Text -> IO ()
new contest = do
  shell
    ( "stack new "
        <> contest
        <> templatePath
        <> resolver
    )
    mempty
  pure ()
