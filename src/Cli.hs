module Cli where

import qualified AtCoder
import qualified Cli.Login                      as Login
import qualified Cli.New                        as New
import qualified Cli.Submit                     as Submit
import           Control.Monad.Except           (runExceptT)
import           Data.Convertible.Utf8.Internal (Text)
import           Options.Applicative

data Command
  = Login
  | New Text
  | Submit Text
  deriving (Show)

parseLogin :: Parser Command
parseLogin = pure Login

parseNew :: Parser Command
parseNew = New <$> argument str (metavar "[CONTEST_NAME]")

parseSubmit :: Parser Command
parseSubmit = Submit <$> argument str (metavar "[TASK_NAME]")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCommand :: Parser Command
parseCommand =
  subparser $
    command "login" (parseLogin `withInfo` "Login AtCoder.")
      <> command "new" (parseNew `withInfo` "Create a new project.")
      <> command "submit" (parseSubmit `withInfo` "Create a new project.")

parseInfo :: ParserInfo Command
parseInfo = parseCommand `withInfo` usage

usage :: String
usage = "stack atcoder [--help] [COMMAND]"

execCommand :: IO ()
execCommand = execParser parseInfo >>= run
  where
    run cmd = case cmd of
      Login    -> Login.login
      New name -> New.new name
      Submit task -> Submit.submit task
