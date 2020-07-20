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
    Right _ -> putStrLn "Login Success!!"
    Left e  -> putStrLn ("Error: Login failed: " <> convert e)

getPassword :: IO Text
getPassword = do
  putStr "Password: "
  hFlush stdout
  hSetEcho stdin False
  password <- getLine
  hSetEcho stdin True
  putStrLn "<Masked password>"
  return $ convert password

getUsername :: IO Text
getUsername = do
  putStr "Username: "
  hFlush stdout
  convert <$> getLine
