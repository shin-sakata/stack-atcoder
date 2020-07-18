module Cli.Login (login) where

import qualified AtCoder
import           Control.Monad.Except           (runExceptT)
import           Data.Convertible.Utf8          (convert)
import           Data.Convertible.Utf8.Internal (Text)
import           Options.Applicative
import           System.IO                      (hFlush, hSetEcho, stdin,
                                                 stdout)
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
