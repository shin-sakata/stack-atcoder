module Cli where

import qualified AtCoder
import           Control.Monad.Except           (runExceptT)
import           Options.Applicative
import qualified Cli.Login as Login
import qualified Cli.New as New

data Command
  = Hello String
  | Login
  | New String
  deriving (Show)

parseHello :: Parser Command
parseHello = Hello <$> argument str (metavar "[NAME]")

parseLogin :: Parser Command
parseLogin = pure Login

parseNew :: Parser Command
parseNew = New <$> argument str (metavar "[CONTEST_NAME]")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCommand :: Parser Command
parseCommand =
  subparser $
    command "hello" (parseHello `withInfo` "Say Hello.")
      <> command "login" (parseLogin `withInfo` "Login AtCoder.")
      <> command "new" (parseNew `withInfo` "Create a new project.")

parseInfo :: ParserInfo Command
parseInfo = parseCommand `withInfo` usage

usage :: String
usage = "stack atcoder [--help] [COMMAND]"

execCommand :: IO ()
execCommand = execParser parseInfo >>= run
  where
    run cmd = case cmd of
      Hello n -> print $ "Hello, " <> n
      Login   -> Login.login
      New name -> New.new name
