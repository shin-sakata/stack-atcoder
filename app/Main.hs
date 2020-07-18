{-# LANGUAGE OverloadedStrings #-}

module Main where

import Cli (execCommand)

main :: IO ()
main = do
  execCommand
