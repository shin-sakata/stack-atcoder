{-# LANGUAGE OverloadedStrings #-}

module Cli where

import qualified Cli.ClearSession               as ClearSession
import qualified Cli.Login                      as Login
import qualified Cli.New                        as New
import           Cli.Result
import qualified Cli.Submit                     as Submit
import           Data.Convertible.Utf8          (convert)
import           Data.Convertible.Utf8.Internal (Text)
import           Options.Applicative

data Command
  = Login
  | New Text
  | Submit Text
  | ClearSession
  deriving (Show)

parseLogin :: Parser Command
parseLogin = pure Login

parseNew :: Parser Command
parseNew = New <$> argument str (metavar "[CONTEST_NAME]")

parseSubmit :: Parser Command
parseSubmit = Submit <$> argument str (metavar "[TASK_NAME]")

parseClearSession :: Parser Command
parseClearSession = pure ClearSession

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCommand :: Parser Command
parseCommand =
  subparser $
    command "login" (parseLogin `withInfo` "Login to AtCoder")
      <> command "new" (parseNew `withInfo` "Create a new project for specified contest")
      <> command "submit" (parseSubmit `withInfo` "Submit solution")
      <> command "clear-session" (parseClearSession `withInfo` "Clear session data (cookie store in HTTP client)")

parseInfo :: ParserInfo Command
parseInfo = parseCommand `withInfo` usage

usage :: String
usage = "stack atcoder [--help] [COMMAND]"

execCommand :: IO ()
execCommand = do
  command <- execParser parseInfo
  run command
  where
    run cmd = case cmd of
      Login        -> do
        result <- runResult Login.login
        handleErr result "Error: Login failed: "
      New name     -> do 
        result <- runResult (New.new name)
        handleErr result "Error: Create project failed: "
      Submit task  -> do
        result <- runResult (Submit.submit task)
        handleErr result "Error: Submit task failed: " 
      ClearSession -> do
        result <- runResult ClearSession.clearSession
        handleErr result "Error: Clear session failed: "

handleErr :: Either Text () -> Text -> IO ()
handleErr (Right _) _ = pure ()
handleErr (Left e) prefix = putStrLn $ convert (prefix <> e)
