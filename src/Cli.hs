module Cli where

import qualified AtCoder
import           Control.Monad.Except           (runExceptT)
import           Options.Applicative
import qualified Cli.Login as Login

data Command
  = Hello String
  | Login
  deriving (Show)

parseHello :: Parser Command
parseHello = Hello <$> argument str (metavar "[NAME]")

parseLogin :: Parser Command
parseLogin = pure Login

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCommand :: Parser Command
parseCommand =
  subparser $
    command "hello" (parseHello `withInfo` "Say Hello")
      <> command "login" (parseLogin `withInfo` "Login AtCoder")

parseInfo :: ParserInfo Command
parseInfo = parseCommand `withInfo` "--help"

execCommand :: IO ()
execCommand = execParser parseInfo >>= run
  where
    run cmd = case cmd of
      Hello n -> print $ "Hello, " <> n
      Login   -> Login.login
