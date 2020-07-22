module Cli.Login (login) where

import qualified AtCoder
import           Cli.Exceptions
import           Data.Convertible.Utf8          (convert)
import           Data.Convertible.Utf8.Internal (Text)
import           System.IO                      (hFlush, hSetEcho, stdin,
                                                 stdout)

login :: (MonadThrow m, MonadIO m) => m ()
login = do
  username <- liftIO getUsername
  password <- liftIO getPassword
  AtCoder.login username password

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
