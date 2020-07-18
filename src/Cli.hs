module Cli where

import qualified AtCoder
import           Control.Monad.Except           (runExceptT)
import           Data.Convertible.Utf8          (convert)
import           Data.Convertible.Utf8.Internal (Text)
import           Options.Applicative
import           System.IO                      (hFlush, hSetEcho, stdin,
                                                 stdout)

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
      Login   -> login

login :: IO ()
login = do
  username <- getUsername
  password <- getPassword
  result <- runExceptT $ AtCoder.login username password
  case result of
    Right _ -> print "Login Success!!"
    Left e  -> print e

getPassword :: IO Text
getPassword = do
  putStr "Enter your password : "
  hFlush stdout
  hSetEcho stdin False
  password <- getLine
  hSetEcho stdin True
  putStrLn "<Masked password>"
  return $ convert password

getUsername :: IO Text
getUsername = do
  putStr "Enter your username : "
  hFlush stdout
  convert <$> getLine
