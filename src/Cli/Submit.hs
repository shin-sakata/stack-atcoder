{-# LANGUAGE OverloadedStrings #-}

module Cli.Submit where

import qualified AtCoder
import Control.Monad.Except (runExceptT)
import Data.Convertible.Utf8 (convert)
import qualified Data.Text as T
import System.Directory as Dir
import qualified Data.Text.IO as T

submit :: T.Text -> IO ()
submit task = do
  contest <- getCurrentContest
  sourceCode <- getSourceCode task
  result <- runExceptT $ AtCoder.submit contest task sourceCode
  case result of
    Right _ -> putStrLn "Submit Success!!"
    Left e -> putStrLn (convert e)

getCurrentContest :: IO T.Text
getCurrentContest = do
  dir <- convert <$> Dir.getCurrentDirectory
  pure $ last (T.split (== '/') dir)

getSourceCode :: T.Text -> IO T.Text
getSourceCode task = T.readFile (convert task <> "/Main.hs")
